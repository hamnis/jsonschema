inThisBuild(
  Seq(
    tlBaseVersion := "0.4",
    startYear := Some(2021),
    organization := "net.hamnaberg",
    crossScalaVersions := Seq("2.13.10", "3.2.2"),
    scalaVersion := crossScalaVersions.value.head,
    ThisBuild / licenses := Seq(License.Apache2),
    developers := List(
      tlGitHubDev("hamnis", "Erlend Hamnaberg")
    )
 )
)

val circeVersion = "0.14.3"

val tapirVersion = "0.3.2"

val catsVersion = "2.9.0"

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .settings(
    name := "jsonschema-core",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.apispec" %%% "apispec-model" % tapirVersion,
      "com.softwaremill.sttp.apispec" %%% "jsonschema-circe" % tapirVersion % Test,
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "io.circe" %%% "circe-jawn" % circeVersion,
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-free" % catsVersion,
      "io.github.cquiroz" %%% "scala-java-time" % "2.5.0",
      "org.scalameta" %%% "munit" % "0.7.29" % Test
    )
  )

lazy val root = tlCrossRootProject.aggregate(core)
