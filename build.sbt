val scala212 = "2.12.18"

val scala213 = "2.13.11"

val scala3 = "3.3.1"

inThisBuild(
  Seq(
    tlBaseVersion := "0.5",
    startYear := Some(2021),
    organizationName := "Erlend Hamnaberg",
    organization := "net.hamnaberg",
    crossScalaVersions := Seq(scala212, scala213, scala3),
    scalaVersion := crossScalaVersions.value.head,
    ThisBuild / licenses := Seq(License.Apache2),
    developers := List(
      tlGitHubDev("hamnis", "Erlend Hamnaberg")
    ),
    tlCiReleaseBranches := Nil,
    tlJdkRelease := Some(11),
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
  )
)

val circeVersion = "0.14.6"

val sttpModelVersion = "0.7.1"

val catsVersion = "2.10.0"

val munitVersion = "1.0.0-M8"

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .settings(
    name := "jsonschema-core",
    headerLicenseStyle := HeaderLicenseStyle.SpdxSyntax,
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.apispec" %%% "apispec-model" % sttpModelVersion,
      "com.softwaremill.sttp.apispec" %%% "jsonschema-circe" % sttpModelVersion % Test,
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "io.circe" %%% "circe-jawn" % circeVersion,
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-free" % catsVersion,
      "org.scalameta" %% "munit" % munitVersion % Test
    )
  )
  .jvmSettings(
    libraryDependencies += "org.jruby.joni" % "joni" % "2.2.1"
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0"
  )

lazy val codegen = crossProject(JVMPlatform, JSPlatform)
  .settings(
    name := "jsonschema-codegen",
    crossScalaVersions := Seq(scala212, scala213),
    headerLicenseStyle := HeaderLicenseStyle.SpdxSyntax,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalameta" % "4.8.12",
      "com.softwaremill.sttp.apispec" %% "jsonschema-circe" % sttpModelVersion,
      "org.scalameta" %% "munit" % munitVersion % Test
    )
  )

lazy val root = tlCrossRootProject.aggregate(core, codegen)
