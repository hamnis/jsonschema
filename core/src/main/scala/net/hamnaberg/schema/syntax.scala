package net.hamnaberg.schema

import sttp.tapir.apispec.{ReferenceOr, Schema => TapirSchema}
import sttp.tapir.openapi.{
  Components => _,
  Operation => TapirOperation,
  MediaType => TapirMediaType,
  PathItem => TapirPathItem,
  _
}

import scala.collection.immutable.ListMap

object syntax {
  object PathItem {
    type Type = TapirPathItem
    val empty: Type = TapirPathItem(None, None, None, None, None, None, None, None, None, None, Nil, Nil)
  }

  object MediaType {
    type Type = TapirMediaType
    def apply(referenceOr: ReferenceOr[TapirSchema]): Type =
      TapirMediaType(schema = Some(referenceOr), example = None, examples = ListMap.empty, encoding = ListMap.empty)
    def apply(): Type =
      TapirMediaType(schema = None, example = None, examples = ListMap.empty, encoding = ListMap.empty)
  }

  object Operation {
    type Type = TapirOperation
    val empty: Type = TapirOperation(Nil, None, None, "", Nil, None, ListMap.empty, None, Nil, Nil)
  }

  implicit class OperationOps(op: Operation.Type) {
    def withId(id: String): Operation.Type = op.copy(operationId = id)

    def request(referenceOr: ReferenceOr[TapirSchema], contentType: String = "application/json"): Operation.Type =
      op.copy(requestBody =
        Some(Right(RequestBody(None, ListMap(contentType -> MediaType(referenceOr)), required = None))))

    def response(referenceOr: ReferenceOr[TapirSchema], contentType: String = "application/json"): Operation.Type =
      op.copy(responses = op.responses.updated(
        ResponsesDefaultKey,
        Right(Response("", ListMap.empty, ListMap(contentType -> MediaType(referenceOr))))
      ))

    def responseStatus(
        status: Int,
        referenceOr: ReferenceOr[TapirSchema],
        contentType: String = "application/json"): Operation.Type =
      op.copy(responses = op.responses.updated(
        ResponsesCodeKey(status),
        Right(Response("", ListMap.empty, ListMap(contentType -> MediaType(referenceOr))))
      ))
  }

  implicit class PathItemOps(item: PathItem.Type) {
    def withSummary(summary: String): PathItem.Type = item.copy(summary = Some(summary))
    def withDescription(description: String): PathItem.Type = item.copy(description = Some(description))

    def withGet[O](components: Components, schemaName: String)(implicit schema: Schema[O]): PathItem.Type =
      item.copy(get = Some(Operation.empty.response(components.referenceToSchema(schemaName).toLeft(schema.compiled))))
    def withPost[I, O](components: Components, schemaName: String)(implicit
        schemaIn: Schema[I],
        schemaOut: Schema[O]): PathItem.Type =
      item.copy(post = Some(
        Operation.empty
          .request(Right(schemaIn.compiled))
          .response(components.referenceToSchema(schemaName).toLeft(schemaOut.compiled))))
    def withPut[I, O](components: Components, schemaName: String)(implicit
        schemaIn: Schema[I],
        schemaOut: Schema[O]): PathItem.Type =
      item.copy(put = Some(
        Operation.empty
          .request(Right(schemaIn.compiled))
          .response(components.referenceToSchema(schemaName).toLeft(schemaOut.compiled))))
    def withDelete: PathItem.Type = item.copy(delete = Some(Operation.empty))
    def withHead: PathItem.Type = item.copy(head = Some(Operation.empty))
  }
}
