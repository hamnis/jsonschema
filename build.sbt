inThisBuild(Seq(
  scalaVersion := "2.13.4",
  testFrameworks += new TestFramework("munit.Framework")
))

val core = project.settings(
  libraryDependencies ++= Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-model" % "0.17.9",
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe" % "0.17.9",
    "io.circe" %% "circe-core" % "0.13.0",
    "org.typelevel" %% "cats-core" % "2.3.1",
    "org.scalameta" %% "munit" % "0.7.21" % Test
  )

)

