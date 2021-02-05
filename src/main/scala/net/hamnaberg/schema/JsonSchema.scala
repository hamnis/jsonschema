package net.hamnaberg.schema

import sttp.tapir.apispec.{Reference, Schema, SchemaType}

import scala.collection.immutable.ListMap

case class JsonSchema[A] private (
    asTapir: Schema,
    fields: List[JsonSchema.FieldRef] = Nil,
    reference: Option[Reference] = None) {
  def withReference(ref: Reference): JsonSchema[A] = copy(reference = Some(ref))
}

object JsonSchema {
  case class FieldRef(name: String, field: Field[_])

  def apply[A](implicit ev: JsonSchema[A]): JsonSchema[A] = ev

  def nonNullable(typ: SchemaType.SchemaType) = Schema(`type` = Some(typ), nullable = Some(false))

  def forProduct1[O, A1](n1: String)(implicit S1: Field[A1]): JsonSchema[O] =
    fromFields(List(FieldRef(n1, S1)))

  def forProduct2[O, A1, A2](n1: String, n2: String)(implicit
      S1: Field[A1],
      S2: Field[A2]): JsonSchema[O] =
    fromFields(List(FieldRef(n1, S1), FieldRef(n2, S2)))

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

  implicit val longSchema: JsonSchema[Long] = embedded(SchemaType.Integer)

  implicit val booleanSchema: JsonSchema[Boolean] = embedded(SchemaType.Boolean)

  implicit val stringSchema: JsonSchema[String] = embedded(SchemaType.String)

  private[schema] def referenceOrSchema[A](implicit ev: Field[A]) =
    ev.reference.toLeft(ev.asTapir)

  private def embedded[A](typ: SchemaType.SchemaType): JsonSchema[A] = JsonSchema(
    Schema(`type` = Some(typ), nullable = Some(false))
  )

}
