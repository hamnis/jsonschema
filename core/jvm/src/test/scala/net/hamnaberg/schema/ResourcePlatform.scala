/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema

import io.circe._
import java.io.{BufferedReader, InputStreamReader, StringWriter}
import java.nio.charset.StandardCharsets

trait ResourcePlatform {
  def readJson(path: String): Either[Error, Json] = {

    val is = getClass.getResourceAsStream(path)
    val reader = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))
    val writer = new StringWriter()
    reader.transferTo(writer)
    parser.decode[Json](writer.toString)
  }
}
