package net.hamnaberg.schema

import net.hamnaberg.schema.NullabilityKnown.NotNull
import sttp.tapir.apispec.{Reference, Schema}

case class Field[A] private[schema] (
    asTapir: Schema,
    reference: Option[Reference] = None,
    nullable: NullabilityKnown = NotNull
) {
  //def map[B](f: A => B): Field[B]
  /*def contramap[B](f: A => B): Field[B] = {

  }*/
}

object Field {
  def apply[A](ev: Field[A]): Field[A] = ev

  implicit def fromSchema[A](implicit ev: JsonSchema[A]): Field[A] =
    Field[A](ev.asTapir, ev.reference)

  implicit def fromOption[A](implicit field: Field[A]): Field[Option[A]] =
    Field(field.asTapir.copy(nullable = Some(true)), None, NullabilityKnown.Null)

  implicit def fromList[A](implicit field: Field[A]): Field[List[A]] =
    Field(
      field.asTapir
        .copy(nullable = Some(false), items = Some(field.reference.toLeft(field.asTapir))),
      None,
      NullabilityKnown.NotNull)
}
