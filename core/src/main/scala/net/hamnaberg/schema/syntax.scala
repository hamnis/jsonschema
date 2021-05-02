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

    def response(
        referenceOr: Option[ReferenceOr[TapirSchema]],
        contentType: String = "application/json"): Operation.Type =
      op.copy(responses = op.responses.updated(
        ResponsesDefaultKey,
        Right(
          Response(
            "",
            ListMap.empty,
            referenceOr.map(ref => ListMap(contentType -> MediaType(ref))).getOrElse(ListMap.empty)))))

    def responseStatus(
        status: Int,
        referenceOr: Option[ReferenceOr[TapirSchema]],
        contentType: String = "application/json"): Operation.Type =
      op.copy(responses = op.responses.updated(
        ResponsesCodeKey(status),
        Right(
          Response(
            "",
            ListMap.empty,
            referenceOr.map(ref => ListMap(contentType -> MediaType(ref))).getOrElse(ListMap.empty)))
      ))
  }

  implicit class PathItemOps(item: PathItem.Type) {
    def withSummary(summary: String): PathItem.Type = item.copy(summary = Some(summary))
    def withDescription(description: String): PathItem.Type = item.copy(description = Some(description))

    def withGet(schema: ReferenceOr[TapirSchema]): PathItem.Type =
      item.copy(get = Some(Operation.empty.response(Some(schema))))
    def withPost(inSchema: ReferenceOr[TapirSchema], outSchema: Option[ReferenceOr[TapirSchema]]): PathItem.Type =
      item.copy(post = Some(Operation.empty.request(inSchema).response(outSchema)))
    def withPost(inSchema: ReferenceOr[TapirSchema], f: TapirOperation => TapirOperation): PathItem.Type =
      item.copy(post = Some(f(Operation.empty.request(inSchema))))
    def withPut(inSchema: ReferenceOr[TapirSchema], outSchema: Option[ReferenceOr[TapirSchema]]): PathItem.Type =
      item.copy(put = Some(Operation.empty.request(inSchema).response(outSchema)))
    def withPut(inSchema: ReferenceOr[TapirSchema], f: TapirOperation => TapirOperation): PathItem.Type =
      item.copy(put = Some(f(Operation.empty.request(inSchema))))
    def withDelete: PathItem.Type = item.copy(delete = Some(Operation.empty))
    def withHead: PathItem.Type = item.copy(head = Some(Operation.empty))
  }
}
