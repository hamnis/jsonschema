/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema

import cats.Eval
import cats.data.{Chain, NonEmptyChain, ValidatedNel}
import cats.syntax.all._
import cats.free.FreeApplicative
import io.circe._
import net.hamnaberg.schema.internal.{encoding, validation}
import sttp.apispec.{ExampleSingleValue, Reference, ReferenceOr, Schema => ApiSpecSchema}

import java.time.{Instant, LocalDate, OffsetDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.util.UUID
import scala.collection.immutable
import scala.util.Try

sealed trait Schema[A] { self =>
  import structure._

  def compiled: ApiSpecSchema = compiled_.value
  def decoder: Decoder[A] = decoder_.value
  def encoder: Encoder[A] = encoder_.value

  def encode(a: A): Json = encoder(a)
  def decode(json: Json): Decoder.Result[A] = decoder.decodeJson(json)
  def validate(json: Json): ValidatedNel[ValidationError, Json] = validation.eval(this, json, Nil)
  def validateDecode(json: Json): ValidatedNel[ValidationError, A] = validation
    .eval(this, json, Nil)
    .andThen(decode(_).fold(err => ValidationError(err.message, err.history).invalidNel, _.validNel))

  def asList(reference: Option[Reference] = None, min: Option[Int] = None, max: Option[Int] = None): Schema[List[A]] =
    Sequence(this, reference, min, max)
  def asVector(
      reference: Option[Reference] = None,
      min: Option[Int] = None,
      max: Option[Int] = None): Schema[Vector[A]] =
    asList(reference, min, max).imap(_.toVector)(_.toList)
  def asSeq(
      reference: Option[Reference] = None,
      min: Option[Int] = None,
      max: Option[Int] = None): Schema[immutable.Seq[A]] =
    asList(reference, min, max).imap(_.toSeq: immutable.Seq[A])(_.toList)

  def reference(ref: Reference): Schema[A] = Custom(Left(ref), encoder, decoder)

  def at(field: String, ref: Option[Reference] = None): Schema[A] =
    Schema.record[A](_(field, identity)(ref.map(this.reference).getOrElse(this)))

  def withDescription(description: String) =
    this match {
      case Meta(schema, meta, _, title) =>
        Meta(schema, meta, Some(description), title)
      case other =>
        Meta(other, None, Some(description), None)
    }

  def withTitle(title: String) =
    this match {
      case Meta(schema, meta, desc, _) =>
        Meta(schema, meta, desc, Some(title))
      case other =>
        Meta(other, None, None, Some(title))
    }

  def withMetaSchema(metaSchema: String) =
    this match {
      case Meta(schema, _, desc, title) =>
        Meta(schema, Some(metaSchema), desc, title)
      case other =>
        Meta(other, Some(metaSchema), None, None)
    }

  def xmap[B](f: A => Decoder.Result[B])(g: B => A): Schema[B] =
    Isos {
      new XMap[B] {
        type Repr = A
        val schema = self
        val r = f
        val w = g
      }
    }

  def imap[B](f: A => B)(g: B => A): Schema[B] =
    Isos {
      new XMap[B] {
        type Repr = A
        val schema = self
        val r = f.andThen(_.asRight)
        val w = g
      }
    }

  val compiled_ : Eval[ApiSpecSchema] = Eval.later(
    internal.ApiSpecModel.schemaFor(this)
  )

  val decoder_ : Eval[Decoder[A]] = Eval.later(
    internal.decoding.fromSchema(this)
  )
  val encoder_ : Eval[Encoder[A]] = Eval.later(
    internal.encoding.fromSchema(this)
  )
}

object Schema {
  import structure._

  def apply[A](implicit S: Schema[A]) = S

  def boundedInt(bounds: Bounds): Schema[Int] =
    SInt(Some("int32"), bounds).xmap(_.toInt.toRight(DecodingFailure("Invalid int", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  def boundedLong(bounds: Bounds): Schema[Long] =
    SInt(Some("int64"), bounds).xmap(_.toLong.toRight(DecodingFailure("Invalid long", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  def boundedDouble(bounds: Bounds): Schema[Double] =
    SNum(Some("double"), bounds).xmap(_.toDouble.asRight)(i => JsonNumber.fromDecimalStringUnsafe(i.toString))
  def boundedBigInt(bounds: Bounds): Schema[BigInt] =
    SInt(None, bounds).xmap(_.toBigInt.toRight(DecodingFailure("Invalid bigint", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))

  def boundedFloat(bounds: Bounds): Schema[Float] =
    SNum(Some("float"), bounds).xmap(_.toFloat.asRight)(i => JsonNumber.fromDecimalStringUnsafe(i.toString))

  def fields[R](p: FreeApplicative[Field[R, *], R]): Schema[R] = Record(p)
  def record[R](
      b: FieldBuilder[R] => FreeApplicative[Field[R, *], R]
  ): Schema[R] =
    fields(b(field))
  def defer[A](schema: => Schema[A]): Schema[A] = Defer(() => schema)

  def custom[A](schema: ApiSpecSchema, encoder: Encoder[A], decoder: Decoder[A]): Schema[A] =
    Custom(Right(schema), encoder, decoder)

  def allOf[A](schemas: NonEmptyChain[Schema[A]]): Schema[A] = AllOf(schemas)
  def alternatives[A](cases: Chain[Alt[A]]): Schema[A] =
    Sum(cases)
  def oneOf[A](b: AltBuilder[A] => Chain[Alt[A]]): Schema[A] =
    alternatives(b(alt))

  def enumeration(options: List[String]) =
    Enumeration(options)

  def field[R] = new FieldBuilder[R]
  def alt[R] = new AltBuilder[R]

  class FieldBuilder[R] {
    def apply[E](
        name: String,
        get: R => E,
        default: Option[E] = None
    )(implicit elemSchema: Schema[E]): FreeApplicative[Field[R, *], E] =
      FreeApplicative.lift(Field.Required(name, elemSchema, default, get))

    def pure[A](a: A): FreeApplicative[Field[R, *], A] = FreeApplicative.pure(a)

    def const[V](
        name: String,
        value: V
    )(implicit valueSchema: Schema[V]): FreeApplicative[Field[R, *], Unit] =
      apply(name, _ => ()) {
        valueSchema.xmap { r =>
          Either.cond((r == value), (), DecodingFailure("Const not equal to self", Nil))
        }(_ => value)
      }

    def opt[E](
        name: String,
        get: R => Option[E]
    )(implicit elemSchema: Schema[E]): FreeApplicative[Field[R, *], Option[E]] =
      FreeApplicative.lift(
        Field.Optional(name, elemSchema, get): Field[R, Option[E]]
      )
  }

  class AltBuilder[A] {
    def apply[B](
        caseSchema_ : Schema[B]
    )(implicit prism_ : Prism[A, B]): Chain[Alt[A]] =
      Chain.one {
        new Alt[A] {
          type Case = B
          val caseSchema = caseSchema_
          val prism = prism_
        }
      }
  }

  implicit val int: Schema[Int] = boundedInt(Bounds.NO)
  implicit val long: Schema[Long] = boundedLong(Bounds.NO)
  implicit val bigInt: Schema[BigInt] = boundedBigInt(Bounds.NO)
  implicit val double: Schema[Double] = boundedDouble(Bounds.NO)
  implicit val float: Schema[Float] = boundedFloat(Bounds.NO)

  implicit val stringInstance: Schema[String] = string
  def string[A]: Schema[String] = Str(None)
  implicit val uuid: Schema[UUID] = Str(Some("uuid")).xmap(s =>
    Try(UUID.fromString(s)).toEither.leftMap(m =>
      DecodingFailure(Option(m.getMessage).getOrElse("Not a valid UUID"), Nil)))(_.toString)

  implicit val instant: Schema[Instant] =
    _dateFormat(DateTimeFormatter.ISO_INSTANT, "date-time", (s, _) => Instant.parse(s))

  implicit val zonedDateTime: Schema[ZonedDateTime] = _dateFormat(
    DateTimeFormatter.ISO_ZONED_DATE_TIME,
    "date-time",
    (s, format) => ZonedDateTime.parse(s, format)
  )

  implicit val offsetDateTime: Schema[OffsetDateTime] = _dateFormat(
    DateTimeFormatter.ISO_OFFSET_DATE_TIME,
    "date-time",
    (s, format) => OffsetDateTime.parse(s, format)
  )

  implicit val localDate: Schema[LocalDate] = _dateFormat(
    DateTimeFormatter.ISO_LOCAL_DATE,
    "date",
    (s, format) => LocalDate.parse(s, format)
  )

  def _dateFormat[A <: TemporalAccessor](
      formatter: DateTimeFormatter,
      typ: String,
      f: (String, DateTimeFormatter) => A) =
    Str(Some(typ)).xmap(s =>
      Try(f(s, formatter)).toEither.leftMap(m =>
        DecodingFailure(Option(m.getMessage).getOrElse(s"Does not parse from ${formatter.toFormat}"), Nil)))(b =>
      formatter.format(b))

  implicit def vector[A](implicit s: Schema[A]): Schema[Vector[A]] = s.asVector()
  implicit def list[A](implicit s: Schema[A]): Schema[List[A]] = s.asList()
  implicit def seq[A](implicit s: Schema[A]): Schema[immutable.Seq[A]] = s.asSeq()
}

object structure {
  final case class SInt(format: Option[String], bounds: Bounds) extends Schema[JsonNumber]
  final case class SNum(format: Option[String], bounds: Bounds) extends Schema[JsonNumber]
  case object SBool extends Schema[Boolean]
  final case class Str(format: Option[String] = None) extends Schema[String]
  final case class Sequence[A](
      value: Schema[A],
      reference: Option[Reference] = None,
      min: Option[Int] = None,
      max: Option[Int] = None)
      extends Schema[List[A]]
  final case class Record[R](value: FreeApplicative[Field[R, *], R]) extends Schema[R]
  final case class Isos[A](value: XMap[A]) extends Schema[A]
  final case class Defer[A](value: () => Schema[A]) extends Schema[A]
  final case class Enumeration(allowed: List[String]) extends Schema[String]
  final case class AllOf[A](value: NonEmptyChain[Schema[A]]) extends Schema[A]
  final case class AnyOf[A](value: NonEmptyChain[Schema[A]]) extends Schema[A]
  final case class Sum[A](value: Chain[Alt[A]]) extends Schema[A]
  final case class Custom[A](_compiled: ReferenceOr[ApiSpecSchema], _encoder: Encoder[A], _decoder: Decoder[A])
      extends Schema[A]
  final case class Meta[A](
      schema: Schema[A],
      metaSchema: Option[String],
      description: Option[String],
      title: Option[String])
      extends Schema[A]

  sealed trait Field[R, E] {
    private[schema] def decode(c: HCursor): Decoder.Result[E]
    private[schema] def encode(obj: R): List[(String, Json)]
    private[schema] def validate(json: JsonObject, history: List[CursorOp]): ValidatedNel[ValidationError, Unit]
    private[schema] def apiSpecSchema: List[(String, ApiSpecSchema)]
  }
  object Field {
    private def write[E](name: String, schema: Schema[E], elem: E): List[(String, Json)] =
      List(name -> internal.encoding.fromSchema(schema).apply(elem))

    final case class Optional[R, E](
        name: String,
        elemSchema: Schema[E],
        get: R => Option[E]
    ) extends Field[R, Option[E]] {
      private[schema] override def decode(c: HCursor): Decoder.Result[Option[E]] = {
        val from = internal.decoding.fromSchema(elemSchema)
        c.get(name)(Decoder.decodeOption(from))
      }

      override private[schema] def encode(obj: R) = {
        val elem = get(obj)
        elem.foldMap(write(name, elemSchema, _))
      }

      override private[schema] def validate(
          json: JsonObject,
          history: List[CursorOp]): ValidatedNel[ValidationError, Unit] = {
        val next = CursorOp.Field(name) :: history
        json(name) match {
          case Some(Json.Null) => ().validNel
          case Some(value) => internal.validation.eval(elemSchema, value, next).map(_ => ())
          case None => ().validNel
        }
      }

      override private[schema] def apiSpecSchema =
        List(name -> internal.ApiSpecModel.schemaFor(elemSchema).copy(nullable = Some(true)))
    }

    final case class Required[R, E](
        name: String,
        elemSchema: Schema[E],
        default: Option[E],
        get: R => E
    ) extends Field[R, E] {
      private[schema] override def decode(c: HCursor) = {
        val down = c.downField(name)
        default match {
          case Some(value) if down.failed => Right(value)
          case _ => down.as(internal.decoding.fromSchema(elemSchema))
        }
      }

      override private[schema] def encode(obj: R) =
        write(name, elemSchema, get(obj))

      override private[schema] def validate(
          json: JsonObject,
          history: List[CursorOp]): ValidatedNel[ValidationError, Unit] = {
        val next = CursorOp.Field(name) :: history
        json(name) match {
          case Some(value) => internal.validation.eval(elemSchema, value, next).map(_ => ())
          case None if default.isDefined => ().validNel
          case None => ValidationError("Not a valid object", next).invalidNel
        }
      }

      override private[schema] def apiSpecSchema = List(
        name -> internal.ApiSpecModel
          .schemaFor(elemSchema)
          .copy(default = default.map(e => ExampleSingleValue(encoding.fromSchema(elemSchema).apply(e).noSpaces))))
    }
  }

  trait Alt[A] {
    type Case
    def caseSchema: Schema[Case]
    def prism: Prism[A, Case]
  }

  trait XMap[A] {
    type Repr
    def schema: Schema[Repr]
    def w: A => Repr
    def r: Repr => Decoder.Result[A]
  }
}
