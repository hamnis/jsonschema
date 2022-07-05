inThisBuild(
  Seq(
    organization := "net.hamnaberg",
    crossScalaVersions := Seq("2.13.8", "3.1.1"),
    scalaVersion := crossScalaVersions.value.head,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-unchecked",
      "-encoding",
      "utf-8",
      "-language:higherKinds"
    )
  )
)

val circeVersion = "0.14.1"

val tapirVersion = "0.2.1"

val catsVersion = "2.8.0"

lazy val core = project
  .settings(CommonSettings.settings)
  .settings(
    name := "openapi-schema-core",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.apispec" %% "openapi-model" % tapirVersion,
      "com.softwaremill.sttp.apispec" %% "openapi-circe" % tapirVersion,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-jawn" % circeVersion,
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-free" % catsVersion,
      "org.scalameta" %% "munit" % "0.7.29" % Test
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
