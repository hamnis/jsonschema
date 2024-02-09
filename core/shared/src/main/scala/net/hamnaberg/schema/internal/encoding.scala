/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema
package internal

import cats._
import cats.data.Chain
import cats.syntax.all._
import cats.free.FreeApplicative
import io.circe.{Encoder, Json, JsonObject}
import io.circe.syntax._

object encoding {
  import structure._

  def fromSchema[A](schema: Schema[A]): Encoder[A] = schema match {
    case Reference(_, schema) =>
      fromSchema(schema)
    case m: Meta[A] =>
      fromSchema(m.schema)
    case SInt(_, _) =>
      Encoder.encodeJsonNumber
    case SNum(_, _) =>
      Encoder.encodeJsonNumber
    case SBool =>
      Encoder.encodeBoolean
    case Str(_, _, _, _) =>
      Encoder.encodeString
    case Enumeration(_) =>
      Encoder.encodeString
    case Sequence(value, _, _) =>
      encodeList(value)
    case Record(rec) =>
      Encoder.instance(encodeObject(rec).andThen(_.asJson))
    case Isos(xmap) =>
      fromSchema(xmap.schema).contramap(xmap.w)
    case Defer(f) => fromSchema(f())
    case Custom(_, encoder, _) => encoder
    case Sum(alts) => encodeAlternatives(alts)
    case AllOf(all, sOpt) =>
      val s = sOpt.getOrElse(all.head)
      fromSchema(s)
    case AnyOf(chain, sOpt) =>
      val s = sOpt.getOrElse(chain.head)
      fromSchema(s)
  }

  def encodeList[A](schema: Schema[A]): Encoder[List[A]] =
    Encoder.encodeList[A](fromSchema[A](schema))

  def encodeObject[R](record: FreeApplicative[Field[R, *], R]) = {
    type Target[A] = R => List[(String, Json)]

    record
      .analyze {
        new (Field[R, *] ~> Target) {
          override def apply[A](fa: Field[R, A]): Target[A] =
            fa.encode
        }
      }
      .andThen(JsonObject.fromIterable(_))
  }

  def encodeAlternatives[A](alts: Chain[Alt[A]]): Encoder[A] = Encoder.instance { superType =>
    implicit def orElse[T]: Monoid[Option[T]] =
      MonoidK[Option].algebra

    val maybe =
      alts.foldMap(alt => alt.prism.tryGet(superType).map(caseValue => fromSchema(alt.caseSchema).apply(caseValue)))
    maybe.getOrElse(sys.error("Unable to create json value from alternative"))
  }

}
