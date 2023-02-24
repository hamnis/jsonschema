# JSON Schema
![Maven Central](https://maven-badges.herokuapp.com/maven-central/net.hamnaberg/jsonschema-core_2.13/badge.svg)

Implements a Json `Schema[A]` using sttp-apispec and Circe.

Circe Decoders and Encoders are derived from the schema definition.

## Background

The idea for this has been in part been due to [Fabio Labella's Dynosaur](https://github.com/SystemFw/dynosaur) and I
have stolen the Prism class from that repo to avoid adding a dependency to monocle. 


### Usage

To use this include a dependency to:

```scala
libraryDependencies += "net.hamnaberg" %% "jsonschema-core" % "version"  
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