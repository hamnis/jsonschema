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

    val api = OpenAPI(
      info = Info("Person api", "0.1"),
      tags = Nil,
      servers = Nil,
      paths = ListMap.empty,
      components = Some(components.underlying),
      security = Nil)

    val modified = api
      .addPathItem(
        "/people/{id}",
        PathItem.empty.withGet(components.referenceToSchema("person").toLeft(Schema[Person].compiled)))
      .addPathItem(
        "/people",
        PathItem.empty
          .withGet(components.referenceToSchema("person").toLeft(peopleWrapper.compiled))
          .withPost(components.referenceToSchema("person").toLeft(Schema[Person].compiled), _.responseStatus(204, None))
      )
    assertEquals(modified.paths.size, 2)
    assert(modified.paths.head._2.get.isDefined)
    assert(modified.paths.tail.head._2.get.isDefined)
    assert(modified.paths.tail.head._2.post.isDefined)
  }
}
