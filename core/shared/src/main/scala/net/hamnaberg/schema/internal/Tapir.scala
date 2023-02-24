/*
 * Copyright 2021 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.hamnaberg.schema
package internal

import cats.free.FreeApplicative
import cats._
import cats.syntax.all._
import sttp.apispec.{ExampleSingleValue, Reference, SchemaType, Schema => TapirSchema}

import scala.collection.immutable.ListMap

object Tapir {
  import structure._

  def schemaFor[A](schema2: Schema[A]): TapirSchema =
    schema2 match {
      case Described(internal, description) =>
        schemaFor(internal).copy(description = Some(description))
      case SInt(format, bounds) =>
        val baseSchema = TapirSchema(`type` = Some(SchemaType.Integer), nullable = Some(false), format = format)
        boundsSchema(baseSchema, bounds)
      case SNum(format, bounds) =>
        val baseSchema = TapirSchema(`type` = Some(SchemaType.Number), nullable = Some(false), format = format)
        boundsSchema(baseSchema, bounds)
      case SBool =>
        TapirSchema(`type` = Some(SchemaType.Boolean), nullable = Some(false))
      case Str(format) =>
        TapirSchema(`type` = Some(SchemaType.String), nullable = Some(false), format = format)
      case Enumeration(allowed) =>
        TapirSchema(
          `type` = Some(SchemaType.String),
          nullable = Some(false),
          `enum` = Some(allowed.map(ExampleSingleValue(_))))
      case Sequence(value, reference, min, max) =>
        TapirSchema(
          `type` = Some(SchemaType.Array),
          nullable = Some(false),
          items = Some(reference.toLeft(schemaFor(value))),
          minItems = min,
          maxItems = max
        )
      case Record(fields) => recordSchema(fields)
      case Isos(xmap) => schemaFor(xmap.schema)
      case Defer(f) => schemaFor(f())
      case Custom(schema, _, _) => TapirSchema(allOf = List(schema))
      case Sum(alts) =>
        TapirSchema(oneOf = alts.map(c => Right(schemaFor(c.caseSchema))).toList)
    }

  def recordSchema[R](fields: FreeApplicative[Field[R, *], R]): TapirSchema = {
    import cats.data.Const

    val value =
      fields
        .foldMap(new (Field[R, *] ~> Const[List[(String, TapirSchema)], *]) {
          override def apply[A](field: Field[R, A]): Const[List[(String, TapirSchema)], A] =
            Const(field.tapirSchema)
        })
        .getConst

    val required = value.collect {
      case (n, compiled) if compiled.nullable.forall(!_) => n
    }

    TapirSchema(
      `type` = Some(SchemaType.Object),
      properties = ListMap.from(value.map { case (name, schema) =>
        (name, schema.asRight[Reference])
      }),
      required = required,
      nullable = Some(false)
    )
  }

  private def boundsSchema(schema: TapirSchema, valid: Bounds) = {
    val minUpdated = valid.min match {
      case None => schema
      case Some(Bound.Inclusive(value)) =>
        schema.copy(minimum = Some(value), exclusiveMinimum = Some(false))
      case Some(Bound.Exclusive(value)) =>
        schema.copy(minimum = Some(value), exclusiveMinimum = Some(true))
    }
    valid.max match {
      case None => schema
      case Some(Bound.Inclusive(value)) =>
        minUpdated.copy(maximum = Some(value), exclusiveMaximum = Some(false))
      case Some(Bound.Exclusive(value)) =>
        minUpdated.copy(maximum = Some(value), exclusiveMaximum = Some(true))
    }
  }
}
