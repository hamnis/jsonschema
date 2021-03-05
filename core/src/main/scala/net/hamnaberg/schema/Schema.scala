package net.hamnaberg.schema

import cats.Eval
import cats.syntax.all._
import cats.free.FreeApplicative
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonNumber}
import sttp.tapir.apispec.{Schema => TapirSchema}

import scala.collection.immutable

sealed trait Schema[A] { self =>
  import Schema._
  import structure._
  def compiled: TapirSchema = compiled_.value
  def decoder: Decoder[A] = decoder_.value
  def encoder: Encoder[A] = encoder_.value

  def encode(a: A): Json = encoder(a)
  def decode(json: Json): Decoder.Result[A] = decoder.decodeJson(json)

  def asList: Schema[List[A]] = Sequence(this)
  def asVector: Schema[Vector[A]] = list(this).imap(_.toVector)(_.toList)
  def asSeq: Schema[immutable.Seq[A]] =
    list(this).imap(_.toSeq: immutable.Seq[A])(_.toList)

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

  def fields[R](p: FreeApplicative[Field[R, *], R]): Schema[R] = Record(p)
  def record[R](
      b: FieldBuilder[R] => FreeApplicative[Field[R, *], R]
  ): Schema[R] =
    fields(b(field))
  def field[R] = new FieldBuilder[R]

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

  implicit val int: Schema[Int] =
    SInt(Some("int32")).xmap(_.toInt.toRight(DecodingFailure("Invalid int", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  implicit val long: Schema[Long] =
    SInt(Some("int64")).xmap(_.toLong.toRight(DecodingFailure("Invalid long", Nil)))(i =>
      JsonNumber.fromIntegralStringUnsafe(i.toString))
  implicit val double: Schema[Double] =
    SNum(Some("double")).xmap(_.toDouble.asRight)(i =>
      JsonNumber.fromDecimalStringUnsafe(i.toString))
  implicit val float: Schema[Float] =
    SNum(Some("float")).xmap(_.toFloat.asRight)(i => JsonNumber.fromDecimalStringUnsafe(i.toString))
  implicit val string: Schema[String] = Str

  implicit def vector[A](implicit s: Schema[A]): Schema[Vector[A]] = s.asVector
  implicit def list[A](implicit s: Schema[A]): Schema[List[A]] = s.asList
  implicit def seq[A](implicit s: Schema[A]): Schema[immutable.Seq[A]] = s.asSeq
  //def defer[A](schema: => Schema[A]): Schema[A] = Defer(() => schema)
}

object structure {
  case class SInt(format: Option[String]) extends Schema[JsonNumber]
  case class SNum(format: Option[String]) extends Schema[JsonNumber]
  case object SBool extends Schema[Boolean]
  case object Str extends Schema[String]
  case class Sequence[A](value: Schema[A], min: Option[Int] = None, max: Option[Int] = None)
      extends Schema[List[A]]
  case class Record[R](value: FreeApplicative[Field[R, *], R]) extends Schema[R]
  case class Isos[A](value: XMap[A]) extends Schema[A]
  //case class Defer[A](value: () => Schema[A]) extends Schema[A]

  trait Field[R, E]
  object Field {
    case class Required[R, E](
        name: String,
        elemSchema: Schema[E],
        get: R => E
    ) extends Field[R, E]

    case class Optional[R, E](
        name: String,
        elemSchema: Schema[E],
        get: R => Option[E]
    ) extends Field[R, Option[E]]
  }

  trait XMap[A] {
    type Repr
    def schema: Schema[Repr]
    def w: A => Repr
    def r: Repr => Decoder.Result[A]
  }
}
