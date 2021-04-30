# OpenAPI Schema
[![Build and Test](https://github.com/hamnis/openapi-schema/actions/workflows/test.yml/badge.svg)](https://github.com/hamnis/openapi-schema/actions/workflows/test.yml)
![Maven Central](https://maven-badges.herokuapp.com/maven-central/net.hamnaberg/openapi-schema-core_2.13/badge.svg)

Implements an OpenAPI Schema for JSON apis using Tapir and Circe.

Circe Decoders and Encoders are derived from the schema.

## Background

The idea for this has been in part been due to [Fabio Labella's Dynosaur](https://github.com/SystemFw/dynosaur) and I
have stolen the Prism class from that repo to avoid adding a dependency to monocle. I find the apis that generate the
whole OpenApi spec to be quite intrusive, so I have opted for a lesser approach.

This could, however, be expanded to include form handling, but since the apis for the different libraries/frameworks are
quite different, I have opted to only handle JSON.

### Usage

To use this include a dependency to:

```scala
libraryDependencies += "net.hamnaberg" %% "openapi-schema-core" % "version"  
```

### Example

```Scala
import net.hamnaberg.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = Schema.record[Person] { field =>
    (
      field[String]("name", _.name),
      field[Int]("age", _.age)(Schema.boundedInt(Bound.Inclusive(0), Bound.Exclusive(150)))
      ).mapN(Person.apply)
  }
}

import io.circe._
import io.circe.syntax._

object Main {
  def main(args: Array[String]) = {
    val json = Json.obj("name" := "Erlend", "age" := 40)
    val Right(result) = schema.decode(json)
    println(result)
  }
}

```