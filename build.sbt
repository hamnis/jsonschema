inThisBuild(
  Seq(
    scalaVersion := "2.13.5",
    testFrameworks += new TestFramework("munit.Framework"),
    addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.11.3").cross(CrossVersion.full))
  ))

lazy val core = project.settings(
  libraryDependencies ++= Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-model" % "0.17.19",
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe" % "0.17.19",
    "io.circe" %% "circe-core" % "0.13.0",
    "org.typelevel" %% "cats-core" % "2.6.0",
    "org.typelevel" %% "cats-free" % "2.6.0",
    "org.scalameta" %% "munit" % "0.7.25" % Test
  )
)
