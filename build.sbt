inThisBuild(
  Seq(
    organization := "net.hamnaberg",
    scalaVersion := "2.13.6",
    testFrameworks += new TestFramework("munit.Framework"),
    addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.13.0").cross(CrossVersion.full))
  ))

lazy val core = project
  .settings(CommonSettings.settings)
  .settings(
    name := "openapi-schema-core",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-openapi-model" % "0.18.0-M11",
      "com.softwaremill.sttp.tapir" %% "tapir-openapi-circe" % "0.18.0-M11",
      "io.circe" %% "circe-core" % "0.13.0",
      "org.typelevel" %% "cats-core" % "2.6.1",
      "org.typelevel" %% "cats-free" % "2.6.1",
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
