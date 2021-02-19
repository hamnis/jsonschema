package net.hamnaberg.schema

import io.circe.{Codec, Decoder, Encoder, Json, Printer}
import sttp.tapir.apispec.{ExampleSingleValue, Reference, Schema, SchemaType}

import java.time.{Instant, LocalDate, LocalDateTime, OffsetDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID
import scala.collection.immutable.ListMap

case class JsonSchema[A] private (
    schema: Schema,
    codec: Codec[A],
    fields: List[JsonSchema.FieldRef] = Nil,
    reference: Option[Reference] = None
) {
  def withReference(ref: Reference): JsonSchema[A] = copy(reference = Some(ref))

  def withExample(example: A, printer: Printer = Printer.spaces2): JsonSchema[A] =
    copy(schema =
      schema.copy(example = Some(ExampleSingleValue(codec.apply(example).printWith(printer)))))

  def encoder: Encoder[A] = codec
  def decoder: Decoder[A] = codec

  def encode(a: A): Json = codec.apply(a)
  def decode(json: Json): Decoder.Result[A] = codec.decodeJson(json)
}

object JsonSchema {
  case class FieldRef(name: String, field: Field[_])

  def apply[A](implicit ev: JsonSchema[A]): JsonSchema[A] = ev

  def forProduct1[O, A1](n1: String)(decode: (A1) => O)(
      encode: O => A1)(implicit F1: Field[A1], C1: Codec[A1]): JsonSchema[O] =
    fromFields(List(FieldRef(n1, F1)), Codec.forProduct1(n1)(decode)(encode))

  def forProduct2[O, A1: Encoder: Decoder, A2: Encoder: Decoder](n1: String, n2: String)(
      decode: (A1, A2) => O)(
      encode: O => (A1, A2))(implicit S1: Field[A1], S2: Field[A2]): JsonSchema[O] =
    fromFields(List(FieldRef(n1, S1), FieldRef(n2, S2)), Codec.forProduct2(n1, n2)(decode)(encode))

  def forProduct3[O, A1: Encoder: Decoder, A2: Encoder: Decoder, A3: Encoder: Decoder](
      n1: String,
      n2: String,
      n3: String)(decode: (A1, A2, A3) => O)(encode: O => (A1, A2, A3))(implicit
      S1: Field[A1],
      S2: Field[A2],
      S3: Field[A3]): JsonSchema[O] =
    fromFields(
      List(FieldRef(n1, S1), FieldRef(n2, S2), FieldRef(n3, S3)),
      Codec.forProduct3(n1, n2, n3)(decode)(encode))

  private[schema] def fromFields[O](fields: List[FieldRef], codec: Codec[O]): JsonSchema[O] = {
    val props =
      ListMap.from(fields.map(f => f.name -> f.field.reference.toLeft(f.field.schema)))

    val required = fields.collect {
      case FieldRef(name, field) if field.nullable == NullabilityKnown.NotNull => name
    }

    val schema =
      Schema(
        `type` = Some(SchemaType.Object),
        nullable = Some(false),
        properties = props,
        required = required)

    JsonSchema[O](schema = schema, codec = codec, fields = fields)
  }

  def enumeration(list: List[String]) = {
    val s = nonNull[String](SchemaType.String)
    s.copy(schema = s.schema.copy(`enum` = Some(list)))
  }

  implicit val localDateTimeSchema: JsonSchema[LocalDateTime] = localDateTimeWithFormatter()
  implicit val localDateSchema: JsonSchema[LocalDate] = localDateWithFormatter()
  implicit val zonedDateTimeSchema: JsonSchema[ZonedDateTime] = zonedDateTimeWithFormatter()
  implicit val offsetDateTimeSchema: JsonSchema[OffsetDateTime] = offsetDateTimeWithFormatter()

  def localDateTimeWithFormatter(
      format: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME) =
    nonNull[LocalDateTime](SchemaType.String, Some("date-time"))(
      Encoder.encodeLocalDateTimeWithFormatter(format),
      Decoder.decodeLocalDateTimeWithFormatter(format))

  def localDateWithFormatter(format: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE) =
    nonNull[LocalDate](SchemaType.String, Some("date"))(
      Encoder.encodeLocalDateWithFormatter(format),
      Decoder.decodeLocalDateWithFormatter(format))

  def zonedDateTimeWithFormatter(
      format: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME) =
    nonNull[ZonedDateTime](SchemaType.String, Some("date-time"))(
      Encoder.encodeZonedDateTimeWithFormatter(format),
      Decoder.decodeZonedDateTimeWithFormatter(format))

  def offsetDateTimeWithFormatter(
      format: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME) =
    nonNull[OffsetDateTime](SchemaType.String, Some("date-time"))(
      Encoder.encodeOffsetDateTimeWithFormatter(format),
      Decoder.decodeOffsetDateTimeWithFormatter(format))

  implicit val instant: JsonSchema[Instant] = nonNull[Instant](SchemaType.String, Some("date-time"))

  implicit val intSchema: JsonSchema[Int] = nonNull(SchemaType.Integer, Some("int32"))

  implicit val doubleSchema: JsonSchema[Double] = nonNull(SchemaType.Number, Some("double"))

  implicit val floatSchema: JsonSchema[Float] = nonNull(SchemaType.Number, Some("float"))

  implicit val bigDecimalSchema: JsonSchema[BigDecimal] = nonNull(SchemaType.Number)

  implicit val bigIntSchema: JsonSchema[BigInt] = nonNull(SchemaType.Number)

  implicit val longSchema: JsonSchema[Long] = nonNull(SchemaType.Integer, Some("int64"))

  implicit val booleanSchema: JsonSchema[Boolean] = nonNull(SchemaType.Boolean)

  implicit val stringSchema: JsonSchema[String] = nonNull(SchemaType.String)

  implicit val uuidSchema: JsonSchema[UUID] = nonNull(SchemaType.String, Some("uuid"))

  def nonNull[A: Encoder: Decoder](
      typ: SchemaType.SchemaType,
      format: Option[String] = None): JsonSchema[A] = JsonSchema(
    Schema(`type` = Some(typ), nullable = Some(false), format = format),
    Codec.from(Decoder[A], Encoder[A])
  )

}
