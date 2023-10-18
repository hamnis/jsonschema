/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schemagen.internal

import scala.meta._

object IsOption {
  def unapply(t: Type): Option[Type] = t match {
    case ta: Type.Apply if ta.tpe.syntax == "Option" => Some(ta.argClause.values.head)
    case _ => None
  }
}
