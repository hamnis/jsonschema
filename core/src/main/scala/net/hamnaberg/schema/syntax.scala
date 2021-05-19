package net.hamnaberg.schema

import sttp.tapir.apispec.{Reference, ReferenceOr, Schema => TapirSchema}
import sttp.tapir.openapi.{
  Components => TapirComponents,
  MediaType => TapirMediaType,
  Operation => TapirOperation,
  PathItem => TapirPathItem,
  _
}

import scala.collection.immutable.ListMap

object syntax {
  implicit class ComponentsOps(val op: TapirComponents) extends AnyVal {
    def addSchemaAndReference[A: Schema](name: String): (Reference, TapirComponents) =
      getSchemaReference(name) -> op.addSchema(name, Schema[A].compiled)

    private def getSchemaReference(name: String) =
      Reference(s"#/components/schemas/$name")
  }

  implicit class OperationOps(val op: TapirOperation) extends AnyVal {
    def request(referenceOr: ReferenceOr[TapirSchema], contentType: String = "application/json"): TapirOperation =
      op.copy(requestBody = Some(
        Right(RequestBody(None, ListMap(contentType -> TapirMediaType(schema = Some(referenceOr))), required = None))))

    def response(
        referenceOr: Option[ReferenceOr[TapirSchema]],
        contentType: String = "application/json"): TapirOperation =
      op.addDefaultResponse(Response.Empty.addMediaType(contentType, TapirMediaType(schema = referenceOr)))

    def responseStatus(
        status: Int,
        referenceOr: Option[ReferenceOr[TapirSchema]],
        contentType: String = "application/json"): TapirOperation =
      op.addResponse(status, Response("", ListMap.empty, ListMap(contentType -> TapirMediaType(schema = referenceOr))))
  }

  implicit class PathItemOps(val item: TapirPathItem) extends AnyVal {

    def withGet(schema: ReferenceOr[TapirSchema]): TapirPathItem =
      item.get(TapirOperation.Empty.responseStatus(200, Some(schema)))
    def withPost(inSchema: ReferenceOr[TapirSchema], outSchema: Option[ReferenceOr[TapirSchema]]): TapirPathItem =
      item.post(TapirOperation.Empty.request(inSchema).responseStatus(200, outSchema))
    def withPost(inSchema: ReferenceOr[TapirSchema], f: TapirOperation => TapirOperation): TapirPathItem =
      item.post(f(TapirOperation.Empty.request(inSchema)))
    def withPut(inSchema: ReferenceOr[TapirSchema], outSchema: Option[ReferenceOr[TapirSchema]]): TapirPathItem =
      item.put(TapirOperation.Empty.request(inSchema).responseStatus(200, outSchema))
    def withPut(inSchema: ReferenceOr[TapirSchema], f: TapirOperation => TapirOperation): TapirPathItem =
      item.put(f(TapirOperation.Empty.request(inSchema)))
  }
}
