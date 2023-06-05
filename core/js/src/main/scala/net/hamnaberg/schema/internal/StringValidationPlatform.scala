/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema.internal

import cats.data.ValidatedNel
import cats.syntax.all._
import io.circe.CursorOp
import net.hamnaberg.schema.ValidationError
import sttp.apispec.{Pattern => SPattern}
import scala.scalajs.js.RegExp

trait StringValidationPlatform {
  def validatePattern(
      input: String,
      pattern: SPattern,
      history: List[CursorOp]): ValidatedNel[ValidationError, String] = {
    val r = RegExp(pattern.value)
    if (r.test(input)) input.validNel
    else
      ValidationError(s"${input} does not match ${pattern}", history).invalidNel
  }
}
