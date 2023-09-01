inThisBuild(
  Seq(
    tlBaseVersion := "0.4",
    startYear := Some(2021),
    organizationName := "Erlend Hamnaberg",
    organization := "net.hamnaberg",
    crossScalaVersions := Seq("2.12.18", "2.13.11", "3.3.0"),
    scalaVersion := crossScalaVersions.value.head,
    ThisBuild / licenses := Seq(License.Apache2),
    developers := List(
      tlGitHubDev("hamnis", "Erlend Hamnaberg")
    ),
    tlCiReleaseBranches := Nil,
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
  )
)

val circeVersion = "0.14.6"

val sttpModelVersion = "0.6.0"

val catsVersion = "2.10.0"

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
      "org.scalameta" %%% "munit" % "0.7.29" % Test
    )
  )
  .jvmSettings(
    libraryDependencies += "org.jruby.joni" % "joni" % "2.2.1"
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0"
  )

lazy val root = tlCrossRootProject.aggregate(core)
