package net.hamnaberg.schema

import io.circe.{CursorOp, Decoder, Encoder, Json}
import io.circe.syntax._
import cats.syntax.all._
import munit.FunSuite
import sttp.tapir.apispec.{ReferenceOr, SchemaType, Schema => TapirSchema}

import scala.collection.immutable.ListMap

class PersonSchemaTest extends FunSuite {
  case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = Schema.record[Person] { field =>
      (
        field[String]("name", _.name),
        field[Int]("age", _.age)
      ).mapN(Person.apply)
    }
  }

  test("person can generate Schema") {
    val expected: TapirSchema = TapirSchema(
      `type` = Some(SchemaType.Object),
      properties = ListMap[String, ReferenceOr[TapirSchema]](
        "name" -> TapirSchema(`type` = Some(SchemaType.String), nullable = Some(false)).asRight,
        "age" -> TapirSchema(`type` = Some(SchemaType.Integer), nullable = Some(false), format = Some("int32")).asRight
      ),
      nullable = Some(false),
      required = List("name", "age")
    )

    val encoder: Encoder[Person] = Person.schema.encoder
    val decoder: Decoder[Person] = Person.schema.decoder

    val original = Person("erlend", 40)
    val Right(decodedPerson) = decoder.decodeJson(encoder.apply(original))
    assertEquals(Person.schema.compiled, expected)
    assertEquals(decodedPerson, original)
  }

  test("validate invalid json") {
    val invalidJson = Json.obj("name" := "John doe")
    val invalidResult = Person.schema.validate(invalidJson)
    assert(invalidResult.isInvalid)

    invalidResult.fold(
      fail => assertEquals(fail.size, 1),
      _ => fail("Nope")
    )
  }
  test("validate invalid json age") {
    val invalidJson = Json.obj("name" := "John doe", "age" := Json.Null)
    val invalidResult = Person.schema.validate(invalidJson)
    assert(invalidResult.isInvalid)

    invalidResult.fold(
      fail => {
        assertEquals(fail.size, 1)
        assertEquals(fail.head.history, List(CursorOp.Field("age")))
      },
      _ => fail("Nope")
    )
  }

  test("validate valid json") {
    val validJson = Json.obj("name" := "John doe", "age" := 18)
    val validResult = Person.schema.validate(validJson)
    assert(validResult.isValid)

    validResult.fold(
      r => fail(s"expected valid $r"),
      j => assert(validJson eq j)
    )
  }

  test("wrapped list") {
    implicit val enc: Encoder[Person] = Person.schema.encoder
    val peopleSchema = Person.schema.asList().at("people")
    val json = Json.obj("people" := List(Person("John Doe", 18), Person("Jane Doe", 19)))
    peopleSchema.decode(json).fold(_ => fail("Invalid"), wrap => assertEquals(wrap.size, 2))
  }
}
