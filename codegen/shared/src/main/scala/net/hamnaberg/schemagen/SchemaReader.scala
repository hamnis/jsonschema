/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schemagen

import java.nio.file.{Files, Path}
import io.circe._
import sttp.apispec.circe._

import scala.io.Source

object SchemaReader {
  case class Error(message: String) extends RuntimeException(message)

  def read(path: Path): Either[Error, TapirSchema] = {
    val data = Files.readString(path)
    parser.decode[TapirSchema](data).left.map(e => Error(e.getMessage))
  }

  def readClasspath(path: String): Either[Error, TapirSchema] = {
    val data = getClass.getResourceAsStream(path)
    val string = Source.fromInputStream(data, "UTF-8").mkString
    parser.decode[TapirSchema](string).left.map(e => Error(e.getMessage))
  }
}
