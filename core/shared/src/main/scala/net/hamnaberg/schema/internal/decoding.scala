/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema
package internal

import cats._
import cats.data.Chain
import cats.free.FreeApplicative
import cats.syntax.all._
import io.circe.Decoder

object decoding {
  import structure._

  def fromSchema[A](schema2: Schema[A]): Decoder[A] =
    schema2 match {
      case Meta(s, _, _, _) =>
        fromSchema(s)
      case SInt(_, _) =>
        Decoder.decodeJsonNumber
      case SNum(_, _) =>
        Decoder.decodeJsonNumber
      case SBool =>
        Decoder.decodeBoolean
      case Str(_, _, _, _) =>
        Decoder.decodeString
      case Enumeration(allowed) =>
        Decoder.decodeString.ensure(e => if (allowed.contains(e)) Nil else List(s"$e, not in $allowed"))
      case Sequence(value, _, _, _) =>
        decodeList(value)
      case Record(record) =>
        decodeRecord(record)
      case Isos(xmap) =>
        Decoder.instance { c =>
          fromSchema(xmap.schema)(c).flatMap(xmap.r)
        }
      case Defer(f) => fromSchema(f())
      case Custom(_, _, decoder) => decoder
      case Sum(alts) => decodeSum(alts)
      case AllOf(all, sOpt) =>
        val s = sOpt.getOrElse(all.head)
        fromSchema(s)
      case AnyOf(nel, sOpt) =>
        val (s, rest) = sOpt.map(s => s -> nel.toChain).getOrElse(nel.head -> nel.tail)
        val head = fromSchema(s)
        rest.foldLeft(head) { case (dec, s) => dec.orElse(fromSchema(s)) }
    }

  def decodeList[A](element: Schema[A]): Decoder[List[A]] =
    Decoder.decodeList(fromSchema[A](element))

  def decodeRecord[R](fields: FreeApplicative[Field[R, *], R]) = Decoder.instance { c =>
    fields.foldMap {
      new (Field[R, *] ~> Decoder.Result[*]) {
        override def apply[A](fa: Field[R, A]): Decoder.Result[A] = fa.decode(c)
      }
    }
  }

  def decodeSum[R](alts: Chain[Alt[R]]): Decoder[R] =
    alts
      .map { alt =>
        fromSchema(alt.caseSchema).map(alt.prism.inject(_))
      }
      .toList
      .reduce(_ orElse _)

}
