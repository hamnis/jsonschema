package net.hamnaberg.schema

import io.circe.{Encoder, Printer}
import sttp.tapir.apispec.{ExampleSingleValue, Reference, Schema, SchemaType}

import scala.collection.immutable.ListMap

case class JsonSchema[A] private (
    asTapir: Schema,
    fields: List[JsonSchema.FieldRef] = Nil,
    reference: Option[Reference] = None) {
  def withReference(ref: Reference): JsonSchema[A] = copy(reference = Some(ref))

  def withExample(example: A, printer: Printer = Printer.spaces2)(implicit
      e: Encoder[A]): JsonSchema[A] =
    copy(asTapir = asTapir.copy(example = Some(ExampleSingleValue(e(example).printWith(printer)))))
}

object JsonSchema {
  case class FieldRef(name: String, field: Field[_])

  def apply[A](implicit ev: JsonSchema[A]): JsonSchema[A] = ev

  def forProduct1[O, A1](n1: String)(implicit S1: Field[A1]): JsonSchema[O] =
    fromFields(List(FieldRef(n1, S1)))

  def forProduct2[O, A1, A2](n1: String, n2: String)(implicit
      S1: Field[A1],
      S2: Field[A2]): JsonSchema[O] =
    fromFields(List(FieldRef(n1, S1), FieldRef(n2, S2)))

  def forProduct3[O, A1, A2, A3](n1: String, n2: String, n3: String)(implicit
      S1: Field[A1],
      S2: Field[A2],
      S3: Field[A3]): JsonSchema[O] =
    fromFields(List(FieldRef(n1, S1), FieldRef(n2, S2), FieldRef(n3, S3)))

  def fromFields[O](fields: List[FieldRef]): JsonSchema[O] = {
    val props =
      ListMap.from(fields.map(f => f.name -> f.field.reference.toLeft(f.field.asTapir)))

    val required = fields.collect {
      case FieldRef(name, field) if field.nullable == NullabilityKnown.NotNull => name
    }

    val schema =
      nonNullable(SchemaType.Object).copy(properties = props, required = required)

    JsonSchema[O](asTapir = schema, fields = fields)
  }

  implicit val intSchema: JsonSchema[Int] = embedded(SchemaType.Integer)

  implicit val doubleSchema: JsonSchema[Double] = embedded(SchemaType.Number)

  implicit val floatSchema: JsonSchema[Float] = embedded(SchemaType.Number)

  implicit val bigDecimalSchema: JsonSchema[BigDecimal] = embedded(SchemaType.Number)

  implicit val bigIntSchema: JsonSchema[BigInt] = embedded(SchemaType.Number)

  implicit val longSchema: JsonSchema[Long] = embedded(SchemaType.Integer)

  implicit val booleanSchema: JsonSchema[Boolean] = embedded(SchemaType.Boolean)

  implicit val stringSchema: JsonSchema[String] = embedded(SchemaType.String)

  private def nonNullable(typ: SchemaType.SchemaType) =
    Schema(`type` = Some(typ), nullable = Some(false))

  private def embedded[A](typ: SchemaType.SchemaType): JsonSchema[A] = JsonSchema(
    Schema(`type` = Some(typ), nullable = Some(false))
  )

}
