package net.hamnaberg.schema

import io.circe.CursorOp

final case class ValidationError(message: String, history: List[CursorOp]) {
  def path = CursorOp.opsToPath(history)
}
