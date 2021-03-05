package net.hamnaberg.schema
package internal

import cats._
import cats.data.Kleisli
import cats.free.FreeApplicative
import io.circe.{Decoder, HCursor}

object decoding {
  import structure._

  def fromSchema[A](schema2: Schema2[A]): Decoder[A] =
    schema2 match {
      case SInt =>
        Decoder.decodeInt
      case SDouble =>
        Decoder.decodeDouble
      case SFloat =>
        Decoder.decodeFloat
      case SLong =>
        Decoder.decodeLong
      case SBool =>
        Decoder.decodeBoolean
      case Str =>
        Decoder.decodeString
      case Sequence(value, _, _) =>
        decodeList(value)
      case Record(record) =>
        decodeRecord(record)
      case Isos(xmap) =>
        Decoder.instance { c =>
          fromSchema(xmap.schema)(c).flatMap(xmap.r)
        }
    }

  def decodeList[A](element: Schema2[A]): Decoder[List[A]] =
    Decoder.decodeList(fromSchema[A](element))

  def decodeRecord[R](fields: FreeApplicative[Field[R, *], R]) = Decoder.instance {
    type Target[A] = Kleisli[Decoder.Result[*], HCursor, A]

    fields.foldMap {
      new (Field[R, *] ~> Target) {
        override def apply[A](fa: Field[R, A]): Target[A] = fa match {
          case Field.Optional(name, elemSchema, _) =>
            Kleisli { c =>
              c.get[Option[A]](name)(
                Decoder.decodeOption(fromSchema[A](elemSchema.asInstanceOf[Schema2[A]])))
            }
          case Field.Required(name, elemSchema, _) =>
            Kleisli { c =>
              c.downField(name).as(fromSchema[A](elemSchema))
            }
        }
      }
    }.run
  }

}
