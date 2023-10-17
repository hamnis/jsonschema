/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema.internal

import cats._
import cats.data._
import cats.free.FreeApplicative
import cats.syntax.all._
import io.circe.{CursorOp, Json, JsonObject}
import net.hamnaberg.schema.structure.Field
import net.hamnaberg.schema.{Schema, ValidationError, structure}

object validation extends StringValidationPlatform {
  def eval[A](schema: Schema[A], json: Json, history: List[CursorOp]): ValidatedNel[ValidationError, Json] =
    schema match {
      case structure.Reference(_, s) => eval(s, json, history)
      case structure.Meta(s, _, _, _) => eval(s, json, history)
      case structure.SInt(Some("int32"), bounds) =>
        val error = ValidationError("Not a valid int", history)
        if (json.isNumber) {
          val num = json.asNumber
          num.flatMap(_.toInt).filter(value => bounds.isWithin(BigDecimal(value))).as(json).toValidNel(error)
        } else {
          error.invalidNel
        }
      case structure.SInt(Some("int64"), bounds) =>
        val error = ValidationError("Not a valid long", history)
        if (json.isNumber) {
          val num = json.asNumber
          num.flatMap(_.toLong).filter(value => bounds.isWithin(BigDecimal(value))).as(json).toValidNel(error)
        } else {
          error.invalidNel
        }
      case structure.SInt(_, bounds) =>
        val error = ValidationError("Not a valid integer", history)
        val left = eval(Schema.int, json, history)
        val right = eval(Schema.long, json, history)
        val bigint = {
          if (json.isNumber) {
            val num = json.asNumber
            num
              .flatMap(_.toBigInt)
              .filter(value => bounds.isWithin(BigDecimal(value)))
              .as(json)
              .toValidNel(error)
          } else {
            error.invalidNel
          }
        }
        left.orElse(right).orElse(bigint).as(json)
      case structure.SNum(_, bounds) =>
        val error = ValidationError("Not a valid numeric", history)
        if (json.isNumber) {
          json.asNumber
            .flatMap(_.toBigDecimal)
            .filterNot(_.isWhole)
            .filter(value => bounds.isWithin(value))
            .as(json)
            .toValidNel(error)
        } else {
          error.invalidNel
        }
      case structure.SBool =>
        val error = ValidationError("Not a valid boolean", history)
        if (json.isBoolean) json.validNel else error.invalidNel
      case structure.Str(format, min, max, pattern) =>
        val error = ValidationError("Not a valid string", history)
        //todo: handle all known formats
        def validate(s: String) = {
          val validMin =
            if (min.forall(length => s.length >= length)) s.validNel[ValidationError]
            else ValidationError(s"Too short string, expected minimum ${min.getOrElse(0)}", history).invalidNel
          val validMax = {
            if (max.forall(length => s.length <= length)) s.validNel[ValidationError]
            else ValidationError(s"Too long string, expected maximum ${max.getOrElse(0)}", history).invalidNel
          }
          val validPattern = pattern.map(p => validatePattern(s, p, history)).getOrElse(json.validNel)
          (validMin, validMax, validPattern).mapN((_, _, _) => json)
        }
        format
          .map(fmt =>
            decoding
              .fromSchema(schema)
              .decodeJson(json)
              .fold(
                err =>
                  ValidationError(
                    s"Unable to decode formatted string using decoder. Format is '${fmt}''",
                    err.history ::: history).invalidNel,
                s => validate(s)
              ))
          .getOrElse {
            json.asString
              .toValidNel[ValidationError](error)
              .andThen(s => validate(s))
          }
      case structure.Sequence(elementSchema, min, max) =>
        val error = ValidationError("Not a valid array", history)
        json.asArray match {
          case Some(array) =>
            val size = array.size
            val validMin =
              min.filter(_ >= size).toValidNel(ValidationError(s"Expected at least ${min.getOrElse(0)} items", history))
            val validMax =
              max.filter(_ <= size).toValidNel(ValidationError(s"Expected at most ${max.getOrElse(0)} items", history))

            val validItems = array.zipWithIndex.traverse { case (item, idx) =>
              eval(elementSchema, item, CursorOp.DownN(idx) :: history).map(_ => ())
            }
            (validMin, validMax, validItems).mapN((_, _, _) => json)
          case None => error.invalidNel
        }
      case structure.Record(fields) =>
        val error = ValidationError("Not a valid object", history)
        json.asObject match {
          case Some(obj) =>
            validateRecord(fields, obj, history).map(_ => json)
          case None => error.invalidNel
        }

      case structure.Isos(xmap) =>
        eval(xmap.schema, json, history).map(_ => json)
      case structure.Defer(s) =>
        eval(s(), json, history)
      case structure.Enumeration(allowed) =>
        val error = ValidationError(s"Not a valid enumeration, expected one of $allowed", history)
        json.asString
          .collect {
            case s if allowed.contains(s) => json
          }
          .toValidNel(error)
      case structure.Sum(alts) =>
        alts.toList.toNel match {
          case Some(nel) =>
            nel.tail
              .foldLeft(
                eval(nel.head.caseSchema, json, history).map(_ => ())
              )((agg, alt) => agg.orElse(eval(alt.caseSchema, json, history).map(_ => ())))
              .map(_ => json)
          case None => ValidationError("No cases for Sum type", history).invalidNel
        }
      case structure.AllOf(chain, optSchema) =>
        val all = chain.traverse_(s => eval(s, json, history)).as(json)
        optSchema.map(s => eval(s, json, history).andThen(_ => all)).getOrElse(all)

      case structure.Custom(_, _, _decoder) =>
        _decoder
          .decodeAccumulating(json.hcursor)
          .fold(
            nel => nel.map(d => ValidationError(d.message, d.history ::: history)).invalid,
            _ => json.validNel
          )
      case structure.AnyOf(chain, optSchema) =>
        val any = chain.tail.foldLeft(eval(chain.head, json, history)) { case (agg, s) =>
          agg.orElse(eval(s, json, history))
        }
        optSchema.map(s => eval(s, json, history).andThen(_ => any)).getOrElse(any)
    }

  def validateRecord[R](fields: FreeApplicative[Field[R, *], R], json: JsonObject, history: List[CursorOp]) =
    fields.foldMap {
      new (Field[R, *] ~> Const[ValidatedNel[ValidationError, Unit], *]) {
        override def apply[A](fa: Field[R, A]): Const[ValidatedNel[ValidationError, Unit], A] =
          Const(fa.validate(json, history))
      }
    }.getConst

}
