package net.hamnaberg.schema

import sttp.tapir.apispec.Reference
import sttp.tapir.openapi.{Components => TapirComponents}

import scala.collection.immutable.ListMap

final case class Components(underlying: TapirComponents = TapirComponents(ListMap.empty, ListMap.empty)) {
  def addSchema[A: Schema](name: String): Components =
    addSchemaAndReference(name)._2

  def addSchemaAndReference[A: Schema](name: String): (Reference, Components) =
    getReference(name) -> Components(
      underlying.copy(schemas = underlying.schemas.updated(name, Right(Schema[A].compiled))))

  def referenceToSchema(name: String): Option[Reference] =
    underlying.schemas.get(name).map(_ => getReference(name))

  private def getReference(name: String) =
    Reference(s"#/components/schemas/$name")
}

object Components {
  val empty = Components()
}
