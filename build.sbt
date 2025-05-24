inThisBuild(
  Seq(
    tlBaseVersion := "0.5",
    startYear := Some(2021),
    organizationName := "Erlend Hamnaberg",
    organization := "net.hamnaberg",
    crossScalaVersions := Seq("2.12.20", "2.13.14", "3.3.3"),
    scalaVersion := crossScalaVersions.value.head,
    ThisBuild / licenses := Seq(License.Apache2),
    developers := List(
      tlGitHubDev("hamnis", "Erlend Hamnaberg")
    ),
    tlCiReleaseBranches := Nil,
    tlJdkRelease := Some(11),
    tlSonatypeUseLegacyHost := true,
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))
  )
)

val circeVersion = "0.14.12"

val sttpModelVersion = "0.11.9"

val catsVersion = "2.12.0"

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
      "org.scalameta" %%% "munit" % "1.1.1" % Test
    )
  )
  .jvmSettings(
    libraryDependencies += "org.jruby.joni" % "joni" % "2.2.6"
  )
  .jsSettings(
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0"
  )

lazy val root = tlCrossRootProject.aggregate(core)
