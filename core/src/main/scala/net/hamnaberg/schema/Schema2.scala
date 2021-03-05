package net.hamnaberg.schema

import cats.Eval
import cats.syntax.all._
import cats.free.FreeApplicative
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonNumber}
import sttp.tapir.apispec.{Schema => TapirSchema}

sealed trait Schema2[A] { self =>
  import Schema2._
  import structure._
  def compiled: TapirSchema = compiled_.value
  def decoder: Decoder[A] = decoder_.value
  def encoder: Encoder[A] = encoder_.value

  def encode(a: A): Json = encoder(a)
  def decode(json: Json): Decoder.Result[A] = decoder.decodeJson(json)

  def asList: Schema2[List[A]] = Sequence(this)
  // def asVector: Schema2[Vector[A]] = list(this).imap(_.toVector)(_.toList)
  //def asSeq: Schema2[immutable.Seq[A]] =
  //  list(this).imap(_.toSeq: immutable.Seq[A])(_.toList)

  /*def read(v: Json): Decoder.Result[A]
  read_(v)

  val read_ : Eval[Json => Decoder.Result[A]] = Eval.later(

  )*/

  def xmap[B](f: A => Decoder.Result[B])(g: B => A): Schema2[B] =
    Isos {
      new XMap[B] {
        type Repr = A
        val schema = self
        val r = f
        val w = g
      }
    }

  def imap[B](f: A => B)(g: B => A): Schema2[B] =
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

object Schema2 {
  import structure._

  def fields[R](p: FreeApplicative[Field[R, *], R]): Schema2[R] = Record(p)
  def record[R](
      b: FieldBuilder[R] => FreeApplicative[Field[R, *], R]
  ): Schema2[R] =
    fields(b(field))
  def field[R] = new FieldBuilder[R]

  class FieldBuilder[R] {
    def apply[E](
        name: String,
        get: R => E
    )(implicit elemSchema: Schema2[E]): FreeApplicative[Field[R, *], E] =
      FreeApplicative.lift(Field.Required(name, elemSchema, get))

    def pure[A](a: A): FreeApplicative[Field[R, *], A] = FreeApplicative.pure(a)

    def const[V](
        name: String,
        value: V
    )(implicit valueSchema: Schema2[V]): FreeApplicative[Field[R, *], Unit] =
      apply(name, _ => ()) {
        valueSchema.xmap { r =>
          Either.cond((r == value), (), DecodingFailure("Const not equal to self", Nil))
        }(_ => value)
      }

    def opt[E](
        name: String,
        get: R => Option[E]
    )(implicit elemSchema: Schema2[E]): FreeApplicative[Field[R, *], Option[E]] =
      FreeApplicative.lift(
        Field.Optional(name, elemSchema, get): Field[R, Option[E]]
      )
  }

  implicit val int: Schema2[Int] = SInt
  implicit val string: Schema2[String] = Str

}

object structure {
  case object SInt extends Schema2[Int]
  case object SDouble extends Schema2[Double]
  case object SFloat extends Schema2[Float]
  case object SLong extends Schema2[Long]
  //case class Num(format: Option[String]) extends Schema2[JsonNumber]
  case object SBool extends Schema2[Boolean]
  case object Str extends Schema2[String]
  case class Sequence[A](value: Schema2[A], min: Option[Int] = None, max: Option[Int] = None)
      extends Schema2[List[A]]
  case class Record[R](value: FreeApplicative[Field[R, *], R]) extends Schema2[R]
  case class Isos[A](value: XMap[A]) extends Schema2[A]

  trait Field[R, E]
  object Field {
    case class Required[R, E](
        name: String,
        elemSchema: Schema2[E],
        get: R => E
    ) extends Field[R, E]

    case class Optional[R, E](
        name: String,
        elemSchema: Schema2[E],
        get: R => Option[E]
    ) extends Field[R, Option[E]]
  }

  trait XMap[A] {
    type Repr
    def schema: Schema2[Repr]
    def w: A => Repr
    def r: Repr => Decoder.Result[A]
  }
}
