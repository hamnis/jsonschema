/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema

import io.circe.CursorOp

final case class ValidationError(message: String, history: List[CursorOp]) {
  def path = CursorOp.opsToPath(history)
}
