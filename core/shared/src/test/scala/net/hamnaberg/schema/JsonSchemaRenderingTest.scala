/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema

import cats.syntax.all._
import io.circe.syntax._
import munit.FunSuite

import sttp.apispec.{Schema => TapirSchema}
import sttp.apispec.circe._

class JsonSchemaRenderingTest extends FunSuite with ResourcePlatform {
  case class Coordinate(latitude: Double, longitude: Double)

  test("schema1") {
    val schema = Schema
      .record[Coordinate] { field =>
        val field1 = field("latitude", _.latitude)(
          Schema.boundedDouble(Bounds.both(Bound.Inclusive.fromInt(-90), Bound.Inclusive.fromInt(90))))
        val field2 = field("longitude", _.longitude)(
          Schema.boundedDouble(Bounds.both(Bound.Inclusive.fromInt(-180), Bound.Inclusive.fromInt(180))))
        (field1, field2).mapN(Coordinate.apply)
      }
      .withDescription("A geographical coordinate.")
      .withTitle("Longitude and Latitude Values")
      .withMetaSchema("https://json-schema.org/draft/2020-12/schema")

    val Right(loaded) = load("/schema1.json")

    val compiled = schema.compiled
    assertEquals(compiled.asJson, loaded.asJson)
  }

  def load(path: String) =
    readJson(path).flatMap(_.as[TapirSchema])
}
