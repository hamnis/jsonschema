/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schemagen

case class PropertyName(name: String, field: String)

object PropertyName {
  private def fieldName(name: String) = name.replaceAll("\\W", "_")

  def apply(name: String): PropertyName = PropertyName(name, fieldName(name))
}
