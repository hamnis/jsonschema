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

final case class Bounds(min: Option[Bound], max: Option[Bound]) {
  def isWithin(value: BigDecimal): Boolean = {
    val lower = min match {
      case None => true
      case Some(Bound.Inclusive(_min)) => value >= _min
      case Some(Bound.Exclusive(_min)) => value > _min
    }
    val upper = max match {
      case None => true
      case Some(Bound.Inclusive(_max)) => value <= _max
      case Some(Bound.Exclusive(_max)) => value < _max
    }
    upper && lower
  }
}

sealed trait Bound

sealed trait BoundCompanion[A] {
  def apply(value: BigDecimal): A
  def fromInt(value: Int): A = apply(BigDecimal(value))
  def fromLong(value: Long): A = apply(BigDecimal(value))
  def fromBigInt(value: Long): A = apply(BigDecimal(value))
  def fromDouble(value: Double): A = apply(BigDecimal(value.toString))
  def fromFloat(value: Float): A = apply(BigDecimal(value.toString))
}

object Bound {
  final case class Inclusive(value: BigDecimal) extends Bound
  object Inclusive extends BoundCompanion[Inclusive]
  final case class Exclusive(value: BigDecimal) extends Bound
  object Exclusive extends BoundCompanion[Exclusive]
}
