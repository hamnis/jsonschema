package net.hamnaberg.schema

import cats.Eval
import cats.data.{Chain, ValidatedNel}
import cats.syntax.all._
import cats.free.FreeApplicative
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonNumber}
import net.hamnaberg.schema.internal.validation
import sttp.tapir.apispec.{Reference, ReferenceOr, Schema => TapirSchema}

import java.time.{Instant, LocalDate, OffsetDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalAccessor
import java.util.UUID
import scala.collection.immutable
import scala.util.Try

sealed trait Schema[A] { self =>
  import Schema._
  import structure._

  def compiled: TapirSchema = compiled_.value
  def decoder: Decoder[A] = decoder_.value
  def encoder: Encoder[A] = encoder_.value

  def encode(a: A): Json = encoder(a)
  def decode(json: Json): Decoder.Result[A] = decoder.decodeJson(json)
  def validate(json: Json): ValidatedNel[ValidationError, Json] = validation.eval(this, json, Nil)
  def validateDecode(json: Json): ValidatedNel[ValidationError, A] = validation
    .eval(this, json, Nil)
    .andThen(decode(_).fold(err => ValidationError(err.message, err.history).invalidNel, _.validNel))

  def withDefault(value: A) = DefaultValue(this, encode(value))

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

  val compiled_ : Eval[TapirSchema] = Eval.later(
    internal.Tapir.schemaFor(this)
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

  def boundedInt(min: Bound, max: Bound): Schema[Int] =
    SInt(Some("int32"), Some(ValidBounds(min, max))).xmap(_.toInt.toRight(DecodingFailure("Invalid int", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  def boundedLong(min: Bound, max: Bound): Schema[Long] =
    SInt(Some("int64"), Some(ValidBounds(min, max))).xmap(_.toLong.toRight(DecodingFailure("Invalid long", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  def boundedDouble(min: Bound, max: Bound): Schema[Double] =
    SNum(Some("double"), Some(ValidBounds(min, max))).xmap(_.toDouble.asRight)(i =>
      JsonNumber.fromDecimalStringUnsafe(i.toString))

  def rangedFloat(min: Bound, max: Bound): Schema[Float] =
    SNum(Some("float"), Some(ValidBounds(min, max))).xmap(_.toFloat.asRight)(i =>
      JsonNumber.fromDecimalStringUnsafe(i.toString))

  def fields[R](p: FreeApplicative[Field[R, *], R]): Schema[R] = Record(p)
  def record[R](
      b: FieldBuilder[R] => FreeApplicative[Field[R, *], R]
  ): Schema[R] =
    fields(b(field))
  def defer[A](schema: => Schema[A]): Schema[A] = Defer(() => schema)

  def custom[A](schema: TapirSchema, encoder: Encoder[A], decoder: Decoder[A]): Schema[A] =
    Custom(Right(schema), encoder, decoder)

  def alternatives[A](cases: Chain[Alt[A]]): Schema[A] =
    Sum(cases)
  def oneOf[A](b: AltBuilder[A] => Chain[Alt[A]]): Schema[A] =
    alternatives(b(alt))

  def field[R] = new FieldBuilder[R]
  def alt[R] = new AltBuilder[R]

  class FieldBuilder[R] {
    def apply[E](
        name: String,
        get: R => E
    )(implicit elemSchema: Schema[E]): FreeApplicative[Field[R, *], E] =
      FreeApplicative.lift(Field.Required(name, elemSchema, get))

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

  implicit val int: Schema[Int] =
    SInt(Some("int32"), None).xmap(_.toInt.toRight(DecodingFailure("Invalid int", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  implicit val long: Schema[Long] =
    SInt(Some("int64"), None).xmap(_.toLong.toRight(DecodingFailure("Invalid long", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  implicit val bigInt: Schema[BigInt] =
    SInt(None, None).xmap(_.toBigInt.toRight(DecodingFailure("Invalid bigint", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  implicit val double: Schema[Double] =
    SNum(Some("double"), None).xmap(_.toDouble.asRight)(i => JsonNumber.fromDecimalStringUnsafe(i.toString))
  implicit val float: Schema[Float] =
    SNum(Some("float"), None).xmap(_.toFloat.asRight)(i => JsonNumber.fromDecimalStringUnsafe(i.toString))
  implicit val string: Schema[String] = Str(None)
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
  final case class SInt(format: Option[String], validRange: Option[ValidBounds]) extends Schema[JsonNumber]
  final case class SNum(format: Option[String], validRange: Option[ValidBounds]) extends Schema[JsonNumber]
  final case object SBool extends Schema[Boolean]
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
  final case class Sum[A](value: Chain[Alt[A]]) extends Schema[A]
  final case class Custom[A](_compiled: ReferenceOr[TapirSchema], _encoder: Encoder[A], _decoder: Decoder[A])
      extends Schema[A]
  final case class DefaultValue[A](schema: Schema[A], default: Json) extends Schema[A]

  trait Field[R, E]
  object Field {
    final case class Required[R, E](
        name: String,
        elemSchema: Schema[E],
        get: R => E
    ) extends Field[R, E]

    final case class Optional[R, E](
        name: String,
        elemSchema: Schema[E],
        get: R => Option[E]
    ) extends Field[R, Option[E]]
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
