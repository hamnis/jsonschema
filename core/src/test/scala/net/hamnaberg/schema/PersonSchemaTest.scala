package net.hamnaberg.schema

import cats.syntax.all._
import io.circe.{Decoder, Encoder}
import munit.FunSuite
import sttp.tapir.apispec.{ReferenceOr, Schema, SchemaType}

import scala.collection.immutable.ListMap

class PersonSchemaTest extends FunSuite {
  case class Person(name: String, age: Int)

  test("person can generate Schema") {
    val expected: Schema = Schema(
      `type` = Some(SchemaType.Object),
      properties = ListMap[String, ReferenceOr[Schema]](
        "name" -> Schema(`type` = Some(SchemaType.String), nullable = Some(false)).asRight,
        "age" -> Schema(
          `type` = Some(SchemaType.Integer),
          nullable = Some(false),
          format = Some("int32")).asRight
      ),
      nullable = Some(false),
      required = List("name", "age")
    )

    /* val schema: JsonSchema[Person] =
      JsonSchema.forProduct2("name", "age")(Person.apply)(Person.unapply(_).get)*/

    val schema2 = Schema2.record[Person] { field =>
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
