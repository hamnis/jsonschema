package net.hamnaberg.schema

case class ValidBounds(min: Bound, max: Bound) {
  def isWithin(value: BigDecimal): Boolean =
    this match {
      case ValidBounds(Bound.Inclusive(_min), Bound.Inclusive(_max)) => value >= _min && value <= _max
      case ValidBounds(Bound.Inclusive(_min), Bound.Exclusive(_max)) => value >= _min && value < _max
      case ValidBounds(Bound.Exclusive(_min), Bound.Inclusive(_max)) => value > _min && value <= _max
      case ValidBounds(Bound.Exclusive(_min), Bound.Exclusive(_max)) => value > _min && value < _max
    }
}

sealed trait Bound

object Bound {
  case class Inclusive(value: BigDecimal) extends Bound
  case class Exclusive(value: BigDecimal) extends Bound
}
