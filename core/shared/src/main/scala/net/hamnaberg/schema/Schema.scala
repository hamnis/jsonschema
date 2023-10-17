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
import net.hamnaberg.schema.Schema.record
import net.hamnaberg.schema.internal.{encoding, validation}
import sttp.apispec.{AnySchema, ExampleSingleValue, Pattern, SchemaLike, Schema => ApiSpecSchema}

import java.time.{Duration, Instant, LocalDate, LocalTime, OffsetDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.util.UUID
import scala.annotation.nowarn
import scala.collection.immutable
import scala.collection.immutable.ListMap
import scala.util.Try

sealed trait Schema[A] extends Product with Serializable { self =>
  import structure._

  def compiled: SchemaLike = compiled_.value
  def decoder: Decoder[A] = decoder_.value
  def encoder: Encoder[A] = encoder_.value

  def encode(a: A): Json = encoder(a)
  def decode(json: Json): Decoder.Result[A] = decoder.decodeJson(json)
  def validate(json: Json): ValidatedNel[ValidationError, Json] = validation.eval(this, json, Nil)
  def validateDecode(json: Json): ValidatedNel[ValidationError, A] = validation
    .eval(this, json, Nil)
    .andThen(decode(_).fold(err => ValidationError(err.message, err.history).invalidNel, _.validNel))

  def asList(min: Option[Int] = None, max: Option[Int] = None): Schema[List[A]] =
    Sequence(this, min, max)
  def asVector(min: Option[Int] = None, max: Option[Int] = None): Schema[Vector[A]] =
    asList(min, max).imap(_.toVector)(_.toList)
  def asSeq(min: Option[Int] = None, max: Option[Int] = None): Schema[immutable.Seq[A]] =
    asList(min, max).imap[immutable.Seq[A]](x => x)(_.toList)

  def reference(ref: String): Schema[A] = Reference(ref, this)

  def at(field: String): Schema[A] =
    Schema.record[A](_(field, identity)(this))

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

  val compiled_ : Eval[SchemaLike] = Eval.later(
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

  def boundedInt(bounds: Bounds[Int]): Schema[Int] =
    SInt(Some("int32"), bounds).xmap(_.toInt.toRight(DecodingFailure("Invalid int", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  def boundedLong(bounds: Bounds[Long]): Schema[Long] =
    SInt(Some("int64"), bounds).xmap(_.toLong.toRight(DecodingFailure("Invalid long", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  def boundedDouble(bounds: Bounds[Double]): Schema[Double] =
    SNum(Some("double"), bounds).xmap(_.toDouble.asRight)(i => JsonNumber.fromDecimalStringUnsafe(i.toString))
  def boundedBigInt(bounds: Bounds[BigInt]): Schema[BigInt] =
    SInt(None, bounds).xmap(_.toBigInt.toRight(DecodingFailure("Invalid bigint", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))

  def boundedFloat(bounds: Bounds[Float]): Schema[Float] =
    SNum(Some("float"), bounds).xmap(_.toFloat.asRight)(i => JsonNumber.fromDecimalStringUnsafe(i.toString))

  def fields[R](p: FreeApplicative[Field[R, *], R]): Schema[R] = Record(p)
  def record[R](
      b: FieldBuilder[R] => FreeApplicative[Field[R, *], R]
  ): Schema[R] =
    fields(b(field))
  def defer[A](schema: => Schema[A]): Schema[A] = Defer(() => schema)

  def custom[A](schema: ApiSpecSchema, encoder: Encoder[A], decoder: Decoder[A]): Schema[A] =
    Custom(schema, encoder, decoder)

  def allOf[A](schemas: NonEmptyChain[Schema[A]], schema: Option[Schema[A]]): Schema[A] = AllOf(schemas, schema)
  def anyOf[A](schemas: NonEmptyChain[Schema[A]], schema: Option[Schema[A]]): Schema[A] = AnyOf(schemas, schema)
  def alternatives[A](cases: Chain[Alt[A]]): Schema[A] =
    Sum(cases)
  def oneOf[A](b: AltBuilder[A] => Chain[Alt[A]]): Schema[A] =
    alternatives(b(alt))

  //TODO: According to https://json-schema.org/draft/2020-12/json-schema-validation.html#section-6.1.2
  //this can be any type
  def enumeration(options: List[String]) =
    Enumeration(options)
  def string[A](
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None,
      pattern: Option[Pattern] = None
  ): Schema[String] = Str(format, minLength, maxLength, pattern)

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

  implicit val int: Schema[Int] = boundedInt(Bounds.empty)
  implicit val long: Schema[Long] = boundedLong(Bounds.empty)
  implicit val bigInt: Schema[BigInt] = boundedBigInt(Bounds.empty)
  implicit val double: Schema[Double] = boundedDouble(Bounds.empty)
  implicit val float: Schema[Float] = boundedFloat(Bounds.empty)

  implicit val stringInstance: Schema[String] = string()
  implicit val uuid: Schema[UUID] = Str(Some("uuid")).xmap(s =>
    Try(UUID.fromString(s)).toEither.leftMap(m =>
      DecodingFailure(Option(m.getMessage).getOrElse("Not a valid UUID"), Nil)))(_.toString)

  implicit val anything: Schema[Json] = Custom(AnySchema.Anything, Encoder[Json], Decoder[Json])

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

  implicit val localTime: Schema[LocalTime] = _dateFormat(
    DateTimeFormatter.ISO_LOCAL_TIME,
    "time",
    (s, format) => LocalTime.parse(s, format)
  )

  implicit val duration: Schema[Duration] =
    Str(Some("duration")).xmap(s =>
      Try(Duration.parse(s)).toEither.leftMap(m =>
        DecodingFailure(Option(m.getMessage).getOrElse(s"Unable to parse $s"), Nil)))(_.toString)

  def _dateFormat[A <: TemporalAccessor](
      formatter: DateTimeFormatter,
      typ: String,
      f: (String, DateTimeFormatter) => A) =
    Str(Some(typ)).xmap(s =>
      Try(f(s, formatter)).toEither.leftMap(m =>
        DecodingFailure(Option(m.getMessage).getOrElse(s"Does not parse from ${formatter.toFormat}"), Nil)))(b =>
      formatter.format(b))

  implicit def unboundedVector[A](implicit s: Schema[A]): Schema[Vector[A]] = s.asVector()
  implicit def unboundedList[A](implicit s: Schema[A]): Schema[List[A]] = s.asList()
  implicit def unboundedSeq[A](implicit s: Schema[A]): Schema[immutable.Seq[A]] = s.asSeq()
}

object structure {
  final case class LocalDefinitions(value: ListMap[String, Schema[_]]) {
    def get(name: String): Option[Schema[_]] = value.get(name)
  }
  object LocalDefinitions {
    val empty = LocalDefinitions(ListMap.empty)
  }

  @nowarn
  final case class SInt[A: Integral](format: Option[String], bounds: Bounds[A]) extends Schema[JsonNumber]
  @nowarn
  final case class SNum[A: Fractional](format: Option[String], bounds: Bounds[A]) extends Schema[JsonNumber]
  case object SBool extends Schema[Boolean]
  final case class Str(
      format: Option[String] = None,
      minLength: Option[Int] = None,
      maxLength: Option[Int] = None,
      pattern: Option[Pattern] = None
  ) extends Schema[String]
  final case class Reference[A](ref: String, schema: Schema[A]) extends Schema[A]
  final case class Sequence[A](value: Schema[A], min: Option[Int] = None, max: Option[Int] = None)
      extends Schema[List[A]]
  final case class Record[R](value: FreeApplicative[Field[R, *], R]) extends Schema[R]
  final case class Isos[A](value: XMap[A]) extends Schema[A]
  final case class Defer[A](value: () => Schema[A]) extends Schema[A]
  //todo: enums may be of any type
  final case class Enumeration(allowed: List[String]) extends Schema[String]
  final case class AllOf[A](value: NonEmptyChain[Schema[A]], targetSchema: Option[Schema[A]]) extends Schema[A]
  final case class AnyOf[A](value: NonEmptyChain[Schema[A]], targetSchema: Option[Schema[A]]) extends Schema[A]
  final case class Sum[A](value: Chain[Alt[A]]) extends Schema[A]
  final case class Custom[A](_compiled: SchemaLike, _encoder: Encoder[A], _decoder: Decoder[A]) extends Schema[A]
  final case class Meta[A](
      schema: Schema[A],
      metaSchema: Option[String],
      description: Option[String],
      title: Option[String])
      extends Schema[A]

  sealed trait Field[R, E] extends Product with Serializable {
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

  trait Alt[A] extends Serializable {
    type Case
    def caseSchema: Schema[Case]
    def prism: Prism[A, Case]
  }

  trait XMap[A] extends Serializable {
    type Repr
    def schema: Schema[Repr]
    def w: A => Repr
    def r: Repr => Decoder.Result[A]
  }
}
