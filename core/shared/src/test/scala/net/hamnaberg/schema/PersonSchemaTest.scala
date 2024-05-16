/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema

import io.circe.{CursorOp, Decoder, Encoder, Json}
import io.circe.syntax._
import cats.syntax.all._
import munit.FunSuite
import sttp.apispec.{SchemaType, Schema => TapirSchema}

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
      `type` = Some(List(SchemaType.Object)),
      properties = ListMap[String, TapirSchema](
        "name" -> TapirSchema(SchemaType.String),
        "age" -> TapirSchema(SchemaType.Integer).copy(format = Some("int32"))
      ),
      required = List("name", "age")
    )

    val encoder: Encoder[Person] = Person.schema.encoder
    val decoder: Decoder[Person] = Person.schema.decoder

    val original = Person("erlend", 40)
    val Right(decodedPerson) = decoder.decodeJson(encoder.apply(original)): @unchecked
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
      j => assertEquals(validJson, j)
    )
  }

  test("wrapped list") {
    implicit val enc: Encoder[Person] = Person.schema.encoder
    val peopleSchema = Person.schema.asList().at("people")
    val json = Json.obj("people" := List(Person("John Doe", 18), Person("Jane Doe", 19)))
    peopleSchema.decode(json).fold(_ => fail("Invalid"), wrap => assertEquals(wrap.size, 2))
  }

  test("default value") {
    val personSchema = Schema
      .record[Person] { field =>
        (
          field[String]("name", _.name),
          field[Int]("age", _.age, Some(0))
        ).mapN(Person.apply)
      }

    val people = List(personSchema.encode(Person("John Doe", 18)), personSchema.encode(Person("Jane Doe", 19)))

    val named = Json.obj("name" := "With Only Name")

    people.foreach { json =>
      assert(personSchema.validate(json).isValid)
    }

    personSchema
      .validateDecode(named)
      .fold(
        err => fail(s"Expected valid, failed with $err"),
        p => assertEquals(p, Person("With Only Name", 0))
      )
  }
}
