scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-openapi-model" % "0.17.9",
  "org.scalameta" %% "munit" % "0.7.21" % Test,
  "io.circe" %% "circe-core" % "0.13.0",
  "org.typelevel" %% "cats-core" % "2.3.1"
)

testFrameworks += new TestFramework("munit.Framework")
