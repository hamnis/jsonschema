inThisBuild(
  Seq(
    scalaVersion := "2.13.4",
    testFrameworks += new TestFramework("munit.Framework"),
    addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.11.3").cross(CrossVersion.full))
  ))

lazy val core = project.settings(
  libraryDependencies ++= Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-model" % "0.17.9",
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe" % "0.17.9",
    "io.circe" %% "circe-core" % "0.13.0",
    "org.typelevel" %% "cats-core" % "2.3.1",
    "org.typelevel" %% "cats-free" % "2.3.1",
    "org.scalameta" %% "munit" % "0.7.21" % Test
  )
)

/*
lazy val magnolia = project
  .dependsOn(core)
  .settings(
    libraryDependencies += "com.propensive" %% "magnolia" % "0.17.0"
  )
 */
