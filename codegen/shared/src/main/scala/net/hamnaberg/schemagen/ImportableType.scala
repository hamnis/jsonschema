/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schemagen

import scala.meta._

case class ImportableType(value: String) {
  val typeName: Type.Name = {
    val arr = value.split('.')
    Type.Name(arr.last)
  }

  val importer: Importer = {
    val arr = value.split('.').toList
    val init = arr.init
    val path = init.tail.foldLeft[Term.Ref](Term.Name(init.head))((t, next) => Term.Select(t, Term.Name(next)))
    Importer(path, List(Importee.Name(Name(arr.last))))
  }
}

object ImportableType {
  val String = ImportableType("java.lang.String")
  val Double = ImportableType("scala.Double")
  val Float = ImportableType("scala.Float")
  val Int = ImportableType("scala.Int")
  val Long = ImportableType("scala.Long")
  val BigInt = ImportableType("scala.BigInt")
  val UUID = ImportableType("java.util.UUID")

  val Option = ImportableType("scala.Option")
  val List = ImportableType("scala.List")

  val Schema = ImportableType("net.hamnaberg.schema.Schema")

  val Instant = ImportableType("java.time.Instant")
  val LocalTime = ImportableType("java.time.LocalTime")
  val LocalDate = ImportableType("java.time.LocalDate")
  val Duration = ImportableType("java.time.Duration")

  val Json = ImportableType("io.circe.Json")

  val never = scala.List(
    String,
    Double,
    Float,
    Int,
    Long,
    BigInt,
    Option,
    List
  )

  val neverImport = never.map(_.importer).toSet

  val defaultFormats = Map[String, ImportableType](
    "uuid" -> UUID,
    "double" -> Double,
    "int64" -> Long,
    "int32" -> Int,
    "date-time" -> Instant,
    "date" -> LocalDate,
    "time" -> LocalTime,
    "duration" -> Duration
  )

  val defaultImports = scala.List(Json)

  val alwaysImport = scala.List(Schema)

  val allImports = alwaysImport ++ defaultImports
}
