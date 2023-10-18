/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema
package internal

import cats.free.FreeApplicative
import cats._
import cats.syntax.all._
import sttp.apispec.{ExampleSingleValue, ExtensionValue, SchemaType, Schema => TapirSchema}

import scala.collection.immutable.ListMap

object ApiSpecModel {
  import structure._

  def schemaFor[A](schema2: Schema[A]): TapirSchema =
    schema2 match {
      case Reference(ref, s) => schemaFor(s).copy($ref = Some(ref))
      case Meta(internal, meta, description, title, extensions) =>
        val ext =
          extensions
            .map(json => ListMap(json.toVector.map { case (k, v) => k -> ExtensionValue(v.noSpaces) }: _*))
            .getOrElse(ListMap.empty)
        schemaFor(internal).copy($schema = meta, description = description, title = title, extensions = ext)
      case SInt(format, bounds) =>
        val baseSchema = TapirSchema(`type` = Some(SchemaType.Integer), nullable = Some(false), format = format)
        boundsSchema(baseSchema, bounds)
      case SNum(format, bounds) =>
        val baseSchema = TapirSchema(`type` = Some(SchemaType.Number), nullable = Some(false), format = format)
        boundsSchema(baseSchema, bounds)
      case SBool =>
        TapirSchema(`type` = Some(SchemaType.Boolean), nullable = Some(false))
      case Str(format, min, max, pattern) =>
        TapirSchema(
          `type` = Some(SchemaType.String),
          nullable = Some(false),
          format = format,
          minLength = min,
          maxLength = max,
          pattern = pattern
        )
      case Enumeration(allowed) =>
        TapirSchema(
          `type` = Some(SchemaType.String),
          nullable = Some(false),
          `enum` = Some(allowed.map(ExampleSingleValue(_))))
      case Sequence(value, min, max) =>
        TapirSchema(
          `type` = Some(SchemaType.Array),
          nullable = Some(false),
          items = Some(schemaFor(value)),
          minItems = min,
          maxItems = max
        )
      case Record(fields) => recordSchema(fields)
      case Isos(xmap) => schemaFor(xmap.schema)
      case Defer(f) => schemaFor(f())
      case Custom(schema, _, _) => TapirSchema(allOf = List(schema))
      case Sum(alts) =>
        TapirSchema(oneOf = alts.map(c => schemaFor(c.caseSchema)).toList)
      case AllOf(schemas, sOpt) =>
        val allOf = TapirSchema(allOf = schemas.map(c => schemaFor(c)).toList)
        sOpt.map(s => schemaFor(s).copy(allOf = allOf.allOf)).getOrElse(allOf)
      case AnyOf(schemas, sOpt) =>
        val any = TapirSchema(anyOf = schemas.map(c => schemaFor(c)).toList)
        sOpt.map(s => schemaFor(s).copy(anyOf = any.anyOf)).getOrElse(any)
    }

  def recordSchema[R](fields: FreeApplicative[Field[R, *], R]): TapirSchema = {
    import cats.data.Const

    val value =
      fields
        .foldMap(new (Field[R, *] ~> Const[List[(String, TapirSchema)], *]) {
          override def apply[A](field: Field[R, A]): Const[List[(String, TapirSchema)], A] =
            Const(field.apiSpecSchema)
        })
        .getConst

    val required = value.collect {
      case (n, compiled) if compiled.nullable.forall(!_) => n
    }

    TapirSchema(
      `type` = Some(SchemaType.Object),
      properties = ListMap(value: _*),
      required = required,
      nullable = Some(false)
    )
  }

  private def boundsSchema[A](schema: TapirSchema, valid: Bounds[A]) = {
    val minUpdated = valid.min match {
      case None => schema
      case Some(i: Bound.Inclusive[A]) =>
        schema.copy(minimum = Some(i.toBigDecimal))
      case Some(e: Bound.Exclusive[A]) =>
        schema.copy(minimum = Some(e.toBigDecimal), exclusiveMinimum = Some(true))
    }
    valid.max match {
      case None => schema
      case Some(i: Bound.Inclusive[A]) =>
        minUpdated.copy(maximum = Some(i.toBigDecimal))
      case Some(e: Bound.Exclusive[A]) =>
        minUpdated.copy(maximum = Some(e.toBigDecimal), exclusiveMaximum = Some(true))
    }
  }
}
