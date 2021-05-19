package net.hamnaberg.schema

import cats.syntax.all._
import munit.FunSuite
import sttp.tapir.openapi.{Components, Info, OpenAPI, PathItem}
import syntax._

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

  test("People full openapi") {
    val (personReference, components) = Components.Empty.addSchemaAndReference[Person]("person")
    val peopleWrapper = Schema[Person].asList(Some(personReference)).at("people")

    val api = OpenAPI(info = Info("Person api", "0.1"))
      .components(components)

    val modified = api
      .addPathItem("/people/{id}", PathItem().withGet(Left(personReference)))
      .addPathItem(
        "/people",
        PathItem()
          .withGet(Right(peopleWrapper.compiled))
          .withPost(Left(personReference), _.responseStatus(204, None))
      )
    assertEquals(modified.paths.pathItems.size, 2)
    assert(modified.paths.pathItems.head._2.get.isDefined)
    assert(modified.paths.pathItems.tail.head._2.get.isDefined)
    assert(modified.paths.pathItems.tail.head._2.post.isDefined)
  }
}
