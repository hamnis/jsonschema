package net.hamnaberg.schema
package internal

import cats.free.FreeApplicative
import cats._
import cats.syntax.all._
import sttp.tapir.apispec.{Reference, Schema => TapirSchema, SchemaType}

import scala.collection.immutable.ListMap

object Tapir {
  import structure._

  def schemaFor[A](schema2: Schema[A]): TapirSchema =
    schema2 match {
      case SInt(format) =>
        TapirSchema(`type` = Some(SchemaType.Integer), nullable = Some(false), format = format)
      case SNum(format) =>
        TapirSchema(`type` = Some(SchemaType.Number), nullable = Some(false), format = format)
      case SBool =>
        TapirSchema(`type` = Some(SchemaType.Boolean), nullable = Some(false))
      case Str(format) =>
        TapirSchema(`type` = Some(SchemaType.String), nullable = Some(false), format = format)
      case Sequence(value, min, max) =>
        TapirSchema(
          `type` = Some(SchemaType.Array),
          nullable = Some(false),
          items = Some(Right(schemaFor(value))),
          minItems = min,
          maxItems = max
        )
      case Record(fields) => recordSchema(fields)
      case Isos(xmap) => schemaFor(xmap.schema)
      case Defer(f) => schemaFor(f())
      case Custom(schema, _, _) => schema
    }

  def recordSchema[R](fields: FreeApplicative[Field[R, *], R]): TapirSchema = {
    import cats.data.Const

    val value =
      fields
        .foldMap(new (Field[R, *] ~> Const[List[(String, TapirSchema)], *]) {
          override def apply[A](field: Field[R, A]): Const[List[(String, TapirSchema)], A] =
            field match {
              case Field.Optional(name, elemSchema, _) =>
                Const(List(name -> schemaFor(elemSchema).copy(nullable = Some(true))))
              case Field.Required(name, elemSchema, _) =>
                Const(List(name -> schemaFor(elemSchema)))
            }
        })
        .getConst

    val required = value.collect {
      case (n, compiled) if compiled.nullable.forall(!_) => n
    }

    TapirSchema(
      `type` = Some(SchemaType.Object),
      properties = ListMap.from(value.map { case (name, schema) =>
        (name, schema.asRight[Reference])
      }),
      required = required,
      nullable = Some(false)
    )
  }
}
