package net.hamnaberg.schema

import cats.syntax.all._
import munit.FunSuite
import sttp.tapir.openapi.{Info, OpenAPI}
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
    val (personReference, components) = Components()
      .addSchemaAndReference[Person]("person")
    val peopleWrapper = Schema[Person].asList(Some(personReference)).at("people")

    val api = OpenAPI(
      info = Info("Person api", "0.1"),
      tags = Nil,
      servers = Nil,
      paths = ListMap.empty,
      components = Some(components.underlying),
      security = Nil)

    val modified = api
      .addPathItem("/people/{id}", PathItem.empty.withGet(Left(personReference)))
      .addPathItem(
        "/people",
        PathItem.empty
          .withGet(Right(peopleWrapper.compiled))
          .withPost(Left(personReference), _.responseStatus(204, None))
      )
    assertEquals(modified.paths.size, 2)
    assert(modified.paths.head._2.get.isDefined)
    assert(modified.paths.tail.head._2.get.isDefined)
    assert(modified.paths.tail.head._2.post.isDefined)
  }
}
