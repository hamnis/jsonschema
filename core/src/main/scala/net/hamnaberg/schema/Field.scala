package net.hamnaberg.schema

import net.hamnaberg.schema.NullabilityKnown.NotNull
import sttp.tapir.apispec.{Reference, Schema}

case class Field[A] private[schema] (
    schema: Schema,
    reference: Option[Reference] = None,
    nullable: NullabilityKnown = NotNull
)

object Field {
  // implicit def apply[A](implicit ev: Field[A]): Field[A] = ev

  implicit def fromSchema[A](implicit ev: JsonSchema[A]): Field[A] =
    Field[A](ev.schema, ev.reference)

  implicit def fromOption[A](implicit field: Field[A]): Field[Option[A]] =
    Field(field.schema.copy(nullable = Some(true)), None, NullabilityKnown.Null)

  implicit def fromList[A](implicit field: Field[A]): Field[List[A]] =
    Field(
      field.schema
        .copy(nullable = Some(false), items = Some(field.reference.toLeft(field.schema))),
      None,
      NullabilityKnown.NotNull)
}
