inThisBuild(
  Seq(
    organization := "net.hamnaberg",
    scalaVersion := "2.13.6",
    testFrameworks += new TestFramework("munit.Framework"),
    addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.13.0").cross(CrossVersion.full))
  ))

val circeVersion = "0.13.0"

val tapirVersion = "0.18.0-M11"

val catsVersion = "2.6.1"

lazy val core = project
  .settings(CommonSettings.settings)
  .settings(
    name := "openapi-schema-core",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-openapi-model" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe" % tapirVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-jawn" % circeVersion,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion,
      "org.scalameta" %% "munit" % "0.7.26" % Test
    )
  )

lazy val root =
  project
    .in(file("."))
    .settings(CommonSettings.settings)
    .settings(
      name := "openapi-schema",
      publishArtifact := false,
      releaseCrossBuild := true,
      releaseVersionBump := sbtrelease.Version.Bump.Minor
    )
    .aggregate(core)
