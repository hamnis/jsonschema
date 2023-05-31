package net.hamnaberg.schema.internal

import cats.data.ValidatedNel
import cats.syntax.all._
import io.circe.CursorOp
import net.hamnaberg.schema.ValidationError
import sttp.apispec.{Pattern => SPattern}
import org.joni.Option
import org.joni.Regex

import java.nio.charset.StandardCharsets

trait StringValidationPlatform {
  def validatePattern(
      input: String,
      pattern: SPattern,
      history: List[CursorOp]): ValidatedNel[ValidationError, String] = {
    val regex = new Regex(pattern.value.getBytes(StandardCharsets.UTF_8))
    val matcher = regex.matcher(input.getBytes(StandardCharsets.UTF_8))
    val result = matcher.search(0, input.length, Option.DEFAULT)
    if (result != -1) input.validNel else ValidationError(s"${input} does not match ${pattern}", history).invalidNel
  }
}
