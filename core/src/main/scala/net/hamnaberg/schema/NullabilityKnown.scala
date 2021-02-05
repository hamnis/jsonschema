package net.hamnaberg.schema

sealed trait NullabilityKnown
object NullabilityKnown {
  object Null extends NullabilityKnown
  object NotNull extends NullabilityKnown
}
