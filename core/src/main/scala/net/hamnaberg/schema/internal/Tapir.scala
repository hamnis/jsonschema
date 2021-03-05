package net.hamnaberg.schema
package internal

import cats.free.FreeApplicative
import cats._
import cats.syntax.all._
import sttp.tapir.apispec.{Reference, Schema, SchemaType}

import scala.collection.immutable.ListMap

object Tapir {
  import structure._

  def schemaFor[A](schema2: Schema2[A]): Schema =
    schema2 match {
      case structure.SInt =>
        Schema(`type` = Some(SchemaType.Integer), nullable = Some(false), format = Some("int32"))
      case structure.SDouble =>
        Schema(`type` = Some(SchemaType.Number), nullable = Some(false), format = Some("double"))
      case structure.SFloat =>
        Schema(`type` = Some(SchemaType.Number), nullable = Some(false), format = Some("float"))
      case structure.SLong =>
        Schema(`type` = Some(SchemaType.Integer), nullable = Some(false), format = Some("int64"))
      case structure.SBool =>
        Schema(`type` = Some(SchemaType.Boolean), nullable = Some(false))
      case structure.Str =>
        Schema(`type` = Some(SchemaType.String), nullable = Some(false))
      case Sequence(value, min, max) =>
        Schema(
          `type` = Some(SchemaType.Array),
          nullable = Some(false),
          items = Some(Right(schemaFor(value))),
          minItems = min,
          maxItems = max
        )
      case Record(fields) => recordSchema(fields)
    }

  def recordSchema[R](fields: FreeApplicative[Field[R, *], R]): Schema = {
    import cats.data.Const

    val value =
      fields
        .foldMap(new (Field[R, *] ~> Const[List[(String, Schema)], *]) {
          override def apply[A](field: Field[R, A]): Const[List[(String, Schema)], A] =
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

    Schema(
      `type` = Some(SchemaType.Object),
      properties = ListMap.from(value.map { case (name, schema) =>
        (name, schema.asRight[Reference])
      }),
      required = required,
      nullable = Some(false)
    )
  }
}
