package net.hamnaberg.schema

import cats.syntax.all._
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
        "age" -> Schema(`type` = Some(SchemaType.Integer), nullable = Some(false)).asRight
      ),
      nullable = Some(false),
      required = List("name", "age")
    )

    val schema: JsonSchema[Person] = JsonSchema.forProduct2[Person, String, Int]("name", "age")
    assertEquals(schema.asTapir, expected)
  }
}
