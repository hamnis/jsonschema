package net.hamnaberg.schema

import cats.syntax.all._
import io.circe.syntax.KeyOps
import io.circe.Json
import munit.FunSuite

class PersonWithAddressTest extends FunSuite {
  case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = Schema.record[Person] { field =>
      (field("name", _.name), field("age", _.age)(Schema.boundedInt(Some(Bound.Inclusive.fromInt(18)), None)))
        .mapN(apply)
    }
  }
  case class Country(name: String, iso: String)
  object Country {
    implicit val schema: Schema[Country] = Schema.record[Country] { field =>
      (field("name", _.name), field("iso", _.iso)(Schema.enumeration(List("no", "se", "uk"))))
        .mapN(apply)
    }

  }
  case class Address(street: String, city: String, country: Country)
  object Address {
    implicit val schema: Schema[Address] = Schema.record[Address] { field =>
      (field("street", _.street), field("city", _.city), field("country", _.country))
        .mapN(apply)
    }

  }
  case class PersonWithAddress(person: Person, address: Address)

  object PersonWithAddress {
    implicit val schema: Schema[PersonWithAddress] = Schema.record[PersonWithAddress] { field =>
      (field("person", _.person), field("address", _.address))
        .mapN(apply)
    }
  }

  test("deep validation") {
    val wrongJson = Json.obj(
      "person" := Json.obj(
        "name" := 19,
        "age" := "Ola dunk"
      ),
      "address" := Json.obj(
        "street" := "John Doe",
        "city" := "London",
        "country" := Json.obj(
          "name" := "uk",
          "iso" := Json.Null
        )
      )
    )
    val result = PersonWithAddress.schema.validate(wrongJson)
    assert(result.isInvalid)
    assertEquals(result.fold(_.size, _ => 0), 3)
    val errors = result.fold(_.toList, _ => Nil)
    assert(errors.exists(_.history.nonEmpty))
  }

}
