package net.hamnaberg.schema
package internal

import cats._
import cats.data.{Chain, Kleisli}
import cats.free.FreeApplicative
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure, HCursor}

object decoding {
  import structure._

  def fromSchema[A](schema2: Schema[A]): Decoder[A] =
    schema2 match {
      case SInt(_, _) =>
        Decoder.decodeJsonNumber
      case SNum(_, _) =>
        Decoder.decodeJsonNumber
      case SBool =>
        Decoder.decodeBoolean
      case Str(_) =>
        Decoder.decodeString
      case Enumeration(allowed) =>
        Decoder.decodeString.ensure(e => if (allowed.contains(e)) Nil else List(s"$e, not in $allowed"))
      case Sequence(value, _, _, _) =>
        decodeList(value)
      case Record(record) =>
        decodeRecord(record)
      case Isos(xmap) =>
        Decoder.instance { c =>
          fromSchema(xmap.schema)(c).flatMap(xmap.r)
        }
      case Defer(f) => fromSchema(f())
      case Custom(_, _, decoder) => decoder
      case Sum(alts) => decodeSum(alts)
    }

  def decodeList[A](element: Schema[A]): Decoder[List[A]] =
    Decoder.decodeList(fromSchema[A](element))

  def decodeRecord[R](fields: FreeApplicative[Field[R, *], R]) = Decoder.instance {
    type Target[A] = Kleisli[Decoder.Result[*], HCursor, A]

    fields.foldMap {
      new (Field[R, *] ~> Target) {
        override def apply[A](fa: Field[R, A]): Target[A] = fa match {
          case Field.Optional(name, elemSchema, _) =>
            Kleisli { c =>
              val from = fromSchema[A](elemSchema.asInstanceOf[Schema[A]])
              c.get[Option[A]](name)(Decoder.decodeOption(from))
            }
          case Field.Required(name, elemSchema, default, _) =>
            Kleisli { c =>
              val down = c.downField(name)
              default match {
                case Some(value) if down.failed => Right(value)
                case _ => down.as(fromSchema[A](elemSchema))
              }
            }
        }
      }
    }.run
  }

  def decodeSum[R](alts: Chain[Alt[R]]): Decoder[R] =
    alts
      .map { alt =>
        fromSchema(alt.caseSchema).map(alt.prism.inject(_))
      }
      .toList
      .reduce(_ orElse _)

}
