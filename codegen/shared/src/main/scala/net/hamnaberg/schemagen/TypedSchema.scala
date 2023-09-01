/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schemagen

import net.hamnaberg.schemagen.internal._
import sttp.apispec.SchemaLike

import scala.meta._

case class TypedSchema(schema: SchemaLike, field: String, typ: Type) {
  def param = Term.Param(Nil, Term.Name(field), Some(typ), None)

  def getEmbeddedTypeName(name: Term.Name): Option[Type.Name] = {
    def getTypeName(t: Type) = t match {
      case Type.Select(n, tpe) if n.syntax == name.syntax => Some(tpe)
      case _ => None
    }

    typ match {
      case IsOption(t) => getTypeName(t)
      case t => getTypeName(t)
    }
  }
}
