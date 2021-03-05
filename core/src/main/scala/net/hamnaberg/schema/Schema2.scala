package net.hamnaberg.schema

import cats.Eval
import cats.free.FreeApplicative
import sttp.tapir.apispec.{Schema => TapirSchema}

sealed trait Schema2[A] { self =>
  import Schema2._
  import structure._
  def compiled: TapirSchema = compiled_.value

  def asList: Schema2[List[A]] = Sequence(this)
  // def asVector: Schema2[Vector[A]] = list(this).imap(_.toVector)(_.toList)
  //def asSeq: Schema2[immutable.Seq[A]] =
  //  list(this).imap(_.toSeq: immutable.Seq[A])(_.toList)

  /*def read(v: Json): Decoder.Result[A]
  read_(v)

  val read_ : Eval[Json => Decoder.Result[A]] = Eval.later(

  )*/

  val compiled_ : Eval[TapirSchema] = Eval.later(
    internal.Tapir.schemaFor(this)
  )
}

object Schema2 {
  import structure._
}

object structure {
  case object SInt extends Schema2[Int]
  case object SDouble extends Schema2[Double]
  case object SFloat extends Schema2[Float]
  case object SLong extends Schema2[Long]
  case object SBool extends Schema2[Boolean]
  case object Str extends Schema2[String]
  case class Sequence[A](value: Schema2[A], min: Option[Int] = None, max: Option[Int] = None)
      extends Schema2[List[A]]
  case class Record[R](value: FreeApplicative[Field[R, *], R]) extends Schema2[R]

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
}
