package net.hamnaberg.schema
package internal

import cats.free.FreeApplicative
import cats._
import cats.syntax.all._
import sttp.tapir.apispec.{ExampleSingleValue, Reference, SchemaType, Schema => TapirSchema}

import scala.collection.immutable.ListMap

object Tapir {
  import structure._

  def schemaFor[A](schema2: Schema[A]): TapirSchema =
    schema2 match {
      case SInt(format, bounds) =>
        val baseSchema = TapirSchema(`type` = Some(SchemaType.Integer), nullable = Some(false), format = format)
        bounds.fold(baseSchema)(boundsSchema(baseSchema, _))
      case SNum(format, bounds) =>
        val baseSchema = TapirSchema(`type` = Some(SchemaType.Number), nullable = Some(false), format = format)
        bounds.fold(baseSchema)(boundsSchema(baseSchema, _))
      case SBool =>
        TapirSchema(`type` = Some(SchemaType.Boolean), nullable = Some(false))
      case Str(format) =>
        TapirSchema(`type` = Some(SchemaType.String), nullable = Some(false), format = format)
      case Enumeration(allowed) =>
        TapirSchema(`type` = Some(SchemaType.String), nullable = Some(false), `enum` = Some(allowed))
      case Sequence(value, reference, min, max) =>
        TapirSchema(
          `type` = Some(SchemaType.Array),
          nullable = Some(false),
          items = Some(reference.toLeft(schemaFor(value))),
          minItems = min,
          maxItems = max
        )
      case Record(fields) => recordSchema(fields)
      case Isos(xmap) => schemaFor(xmap.schema)
      case Defer(f) => schemaFor(f())
      case Custom(schema, _, _) => TapirSchema(allOf = List(schema))
      case Sum(alts) =>
        TapirSchema(oneOf = alts.map(c => Right(schemaFor(c.caseSchema))).toList)
      case DefaultValue(schema, default) => schemaFor(schema).copy(default = Some(ExampleSingleValue(default.noSpaces)))
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

  private def boundsSchema(schema: TapirSchema, valid: ValidBounds) = valid match {
    case ValidBounds(Bound.Inclusive(min), Bound.Inclusive(max)) =>
      schema.copy(
        minimum = Some(min),
        exclusiveMinimum = Some(false),
        maximum = Some(max),
        exclusiveMaximum = Some(false))
    case ValidBounds(Bound.Exclusive(min), Bound.Inclusive(max)) =>
      schema.copy(
        minimum = Some(min),
        exclusiveMinimum = Some(true),
        maximum = Some(max),
        exclusiveMaximum = Some(false))
    case ValidBounds(Bound.Inclusive(min), Bound.Exclusive(max)) =>
      schema.copy(
        minimum = Some(min),
        exclusiveMinimum = Some(false),
        maximum = Some(max),
        exclusiveMaximum = Some(true))
    case ValidBounds(Bound.Exclusive(min), Bound.Exclusive(max)) =>
      schema.copy(
        minimum = Some(min),
        exclusiveMinimum = Some(true),
        maximum = Some(max),
        exclusiveMaximum = Some(true))
  }
}
