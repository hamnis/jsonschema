package net.hamnaberg.schema

import sttp.tapir.openapi.{Components => TapirComponents}

import scala.collection.immutable.ListMap

case class Components(underlying: TapirComponents = TapirComponents(ListMap.empty, ListMap.empty)) {
  def addSchema[A: Schema](name: String) =
    Components(underlying.copy(schemas = underlying.schemas.updated(name, Right(Schema[A].compiled))))
}
