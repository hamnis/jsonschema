/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schemagen

import munit.FunSuite

class GenerateCaseClassTest extends FunSuite {

  test("Generate from schema1.json") {
    val schema = SchemaReader.readClasspath("/schema1.json")
    val expected =
      """package foo.bar
        |import net.hamnaberg.schema.Schema
        |final case class Coordinate(latitude: Double, longitude: Double)
        |object Coordinate {
        |  implicit val schema: Schema[Coordinate] = Schema.record {
        |    field => (field("latitude", _.latitude)(Schema.boundedDouble(Bounds.both(Bound.Inclusive(-90.0d), Bound.Inclusive(90.0d)))), field("longitude", _.longitude)(Schema.boundedDouble(Bounds.both(Bound.Inclusive(-180.0d), Bound.Inclusive(180.0d))))).mapN(apply)
        |  }
        |}""".stripMargin

    val cc = schema.left
      .map(_.message)
      .flatMap(schema =>
        GenerateCaseClass.generate(schema, "Coordinate", ImportableType.allImports, ImportableType.defaultFormats))
    val result = cc.map { generated =>
      generated.asSyntax("foo.bar")
    }

    assert(cc.isRight)
    assertEquals(result, Right(expected))
  }

  test("Generate from schema2.json") {
    val schema = SchemaReader.readClasspath("/schema2.json")
    val expected =
      """|package bar
         |import net.hamnaberg.schema.Schema
         |import java.util.UUID
         |final case class Plug(title: String, styling: String, index: Int, content: Option[Plug.Content])
         |object Plug {
         |  implicit val schema: Schema[Plug] = Schema.record {
         |    field => (field("title", _.title), field("styling", _.styling), field("index", _.index)(Schema.boundedInt(Bounds.min(Bound.Inclusive(1)))), field("content", _.content)).mapN(apply)
         |  }
         |  final case class Content(id: UUID, `type`: String, source: Option[String])
         |  object Content {
         |    implicit val schema: Schema[Content] = Schema.record {
         |      field => (field("id", _.id), field("type", _.`type`), field("source", _.source)).mapN(apply)
         |    }
         |  }
         |}""".stripMargin

    val cc = schema.left
      .map(_.message)
      .flatMap(schema =>
        GenerateCaseClass.generate(schema, "Plug", ImportableType.allImports, ImportableType.defaultFormats))
    val result = cc.map { cls =>
      cls.asSyntax("bar")
    }

    assert(cc.isRight)
    assertEquals(result, Right(expected))
  }

}
