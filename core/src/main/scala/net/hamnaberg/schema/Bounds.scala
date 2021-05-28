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
