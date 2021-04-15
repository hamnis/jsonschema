package net.hamnaberg

import cats.data.ValidatedNel
import io.circe.Json

package object schema {
  type Validation[A] = ValidatedNel[ValidationError, (Schema[A], Json)]
}
