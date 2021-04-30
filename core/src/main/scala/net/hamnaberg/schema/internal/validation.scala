package net.hamnaberg.schema.internal

import cats._
import cats.data._
import cats.free.FreeApplicative
import cats.syntax.all._
import io.circe.{CursorOp, Json, JsonObject}
import net.hamnaberg.schema.structure.Field
import net.hamnaberg.schema.{Schema, ValidBounds, ValidationError, structure}

object validation {
  def eval[A](schema: Schema[A], json: Json, history: List[CursorOp]): ValidatedNel[ValidationError, Json] =
    schema match {
      case structure.SInt(Some("int32"), range) =>
        val error = ValidationError("Not a valid int", history)
        if (json.isNumber) {
          val num = json.asNumber
          num.flatMap(_.toInt).filter(value => range.forall(_.isWithin(BigDecimal(value)))).as(json).toValidNel(error)
        } else {
          error.invalidNel
        }
      case structure.SInt(Some("int64"), range) =>
        val error = ValidationError("Not a valid long", history)
        if (json.isNumber) {
          val num = json.asNumber
          num.flatMap(_.toLong).filter(value => range.forall(_.isWithin(BigDecimal(value)))).as(json).toValidNel(error)
        } else {
          error.invalidNel
        }
      case structure.SInt(_, range) =>
        val error = ValidationError("Not a valid integer", history)
        val left = eval(Schema.int, json, history)
        val right = eval(Schema.long, json, history)
        val bigint = {
          if (json.isNumber) {
            val num = json.asNumber
            num
              .flatMap(_.toBigInt)
              .filter(value => range.forall(_.isWithin(BigDecimal(value))))
              .as(json)
              .toValidNel(error)
          } else {
            error.invalidNel
          }
        }
        left.orElse(right).orElse(bigint).as(json)
      case structure.SNum(_, range) =>
        val error = ValidationError("Not a valid numeric", history)
        if (json.isNumber) {
          json.asNumber
            .flatMap(_.toBigDecimal)
            .filterNot(_.isWhole)
            .filter(value => range.forall(_.isWithin(value)))
            .as(json)
            .toValidNel(error)
        } else {
          error.invalidNel
        }
      case structure.SBool =>
        val error = ValidationError("Not a valid boolean", history)
        if (json.isBoolean) json.validNel else error.invalidNel
      case structure.Str(None) =>
        val error = ValidationError("Not a valid string", history)
        json.asString.as(json).toValidNel(error)

      case structure.Str(Some(frmt)) =>
        decoding
          .fromSchema(schema)
          .decodeJson(json)
          .fold(
            e => ValidationError(s"Not a valid formatted string ($frmt)", history ++ e.history).invalidNel,
            _ => json.validNel
          )

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
        if (json.isString)
          json.asString.filter(allowed.contains).as(json).toValidNel(error)
        else error.invalidNel
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
      case structure.Custom(_, _, _decoder) =>
        _decoder
          .decodeAccumulating(json.hcursor)
          .fold(
            nel => nel.map(d => ValidationError(d.message, history ++ d.history)).invalid,
            _ => json.validNel
          )
    }

  def validateRecord[R](fields: FreeApplicative[Field[R, *], R], json: JsonObject, history: List[CursorOp]) =
    fields.foldMap {
      new (Field[R, *] ~> Const[ValidatedNel[ValidationError, Unit], *]) {
        override def apply[A](fa: Field[R, A]): Const[ValidatedNel[ValidationError, Unit], A] = fa match {
          case Field.Optional(name, elemSchema, _) =>
            val next = CursorOp.Field(name) :: history
            Const(json(name) match {
              case Some(Json.Null) => ().validNel
              case Some(value) => eval(elemSchema, value, next).map(_ => ())
              case None => ().validNel
            })
          case Field.Required(name, elemSchema, _) =>
            val next = CursorOp.Field(name) :: history
            json(name) match {
              case Some(value) => Const(eval(elemSchema, value, next).map(_ => ()))
              case None => Const(ValidationError("Not a valid object", next).invalidNel)
            }
        }
      }
    }.getConst

}
