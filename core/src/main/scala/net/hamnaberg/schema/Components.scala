package net.hamnaberg.schema

import sttp.tapir.apispec.Reference
import sttp.tapir.openapi.{Components => TapirComponents}

import scala.collection.immutable.ListMap

case class Components(underlying: TapirComponents = TapirComponents(ListMap.empty, ListMap.empty)) {
  def addSchema[A: Schema](name: String): Components =
    Components(underlying.copy(schemas = underlying.schemas.updated(name, Right(Schema[A].compiled))))

  def referenceToSchema(name: String): Option[Reference] =
    underlying.schemas.get(name).map(_ => Reference(s"#/components/schemas/$name"))
}
