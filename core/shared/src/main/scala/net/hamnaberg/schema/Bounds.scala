/*
 * Copyright 2021 Erlend Hamnaberg
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package net.hamnaberg.schema

import scala.annotation.nowarn

@nowarn
final case class Bounds[A: Numeric](min: Option[Bound[A]], max: Option[Bound[A]]) {
  private[schema] def isWithin(value: BigDecimal): Boolean = {
    val lower = min.forall {
      case i: Bound.Inclusive[A] => value >= i.toBigDecimal
      case e: Bound.Exclusive[A] => value > e.toBigDecimal
    }
    val upper = max.forall {
      case i: Bound.Inclusive[A] => value <= i.toBigDecimal
      case e: Bound.Exclusive[A] => value < e.toBigDecimal
    }
    upper && lower
  }
}

object Bounds {
  def empty[A: Numeric]: Bounds[A] = Bounds(None, None)

  def both[A: Numeric](min: Bound[A], max: Bound[A]): Bounds[A] = apply(Some(min), Some(max))

  def min[A: Numeric](min: Bound[A]): Bounds[A] = Bounds(Some(min), None)
  def max[A: Numeric](max: Bound[A]): Bounds[A] = Bounds(None, Some(max))
}

sealed trait Bound[A] {
  def value: A
  private[schema] def toBigDecimal: BigDecimal = BigDecimal(value.toString)
}
object Bound {
  @nowarn
  final case class Inclusive[A: Numeric](value: A) extends Bound[A]
  @nowarn
  final case class Exclusive[A: Numeric](value: A) extends Bound[A]
}
