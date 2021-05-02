package net.hamnaberg.schema

import cats.syntax.all._
import munit.FunSuite
import io.circe.syntax._
import sttp.tapir.apispec.Reference
import sttp.tapir.openapi.{Info, OpenAPI}
import sttp.tapir.openapi.circe._
import syntax._

import scala.collection.immutable.ListMap

class OpenApiTest extends FunSuite {
  case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = Schema.record[Person] { field =>
      (
        field[String]("name", _.name),
        field[Int]("age", _.age)
      ).mapN(Person.apply)
    }
  }

  test("People form") {
    val components = Components()
      .addSchema[Person]("person")
    val peopleWrapper = Schema[Person].asList(components.referenceToSchema("person")).wrapper(None, "people")
    println(peopleWrapper.compiled)
    //.addSchema("people")(peopleWrapper)

    val api = OpenAPI(
      info = Info("Person api", "0.1"),
      tags = Nil,
      servers = Nil,
      paths = ListMap.empty,
      components = Some(components.underlying),
      security = Nil)

    val modified = api
      .addPathItem("/people/{id}", PathItem.empty.withGet[Person](components, "person"))
      .addPathItem("/people", PathItem.empty.withGet(components, "people")(peopleWrapper))
    println(modified.asJson.spaces2)
  }
}
