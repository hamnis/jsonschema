package net.hamnaberg.schema

import io.circe.CursorOp

case class ValidationError(message: String, history: List[CursorOp])
