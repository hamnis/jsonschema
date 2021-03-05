package net.hamnaberg.schema

import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import munit.FunSuite
import sttp.tapir.apispec.{ReferenceOr, Schema => TapirSchema, SchemaType}

import scala.collection.immutable.ListMap

class PersonSchemaTest extends FunSuite {
  case class Person(name: String, age: Int)

  test("person can generate Schema") {
    val expected: TapirSchema = TapirSchema(
      `type` = Some(SchemaType.Object),
      properties = ListMap[String, ReferenceOr[TapirSchema]](
        "name" -> TapirSchema(`type` = Some(SchemaType.String), nullable = Some(false)).asRight,
        "age" -> TapirSchema(
          `type` = Some(SchemaType.Integer),
          nullable = Some(false),
          format = Some("int32")).asRight
      ),
      nullable = Some(false),
      required = List("name", "age")
    )

    val schema2 = Schema.record[Person] { field =>
      (
        field[String]("name", _.name),
        field[Int]("age", _.age)
      ).mapN(Person.apply)
    }

    val encoder: Encoder[Person] = schema2.encoder
    val decoder: Decoder[Person] = schema2.decoder

    val original = Person("erlend", 40)
    val Right(decodedPerson) = decoder.decodeJson(encoder.apply(original))
    assertEquals(schema2.compiled, expected)
    assertEquals(decodedPerson, original)
  }
}
