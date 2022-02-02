import sbt._, Keys._
import com.jsuereth.sbtpgp.PgpKeys
import sbtrelease.ReleasePlugin.autoImport._

object CommonSettings {
  val settings = Seq(
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) =>
        Seq(compilerPlugin(("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full)))
      case _ => Nil
    }),
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((3, _)) => Seq("-Ykind-projector")
      case Some((2, v)) if v <= 12 => Seq("-Ypartial-unification")
      case _ => Seq.empty
    }),
    Compile / compile / scalacOptions ++= Seq("-release", "8"),
    Test / scalacOptions ++= Seq("-release", "11"),
    publishTo := {
      if (isSnapshot.value)
        Some(Opts.resolver.sonatypeSnapshots)
      else
        Some(Opts.resolver.sonatypeStaging)
    },
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    packageOptions += {
      val title = name.value
      val ver = version.value
      val vendor = organization.value

      Package.ManifestAttributes(
        "Created-By" -> "Scala Build Tool",
        "Built-By" -> System.getProperty("user.name"),
        "Build-Jdk" -> System.getProperty("java.version"),
        "Specification-Title" -> title,
        "Specification-Version" -> ver,
        "Specification-Vendor" -> vendor,
        "Implementation-Title" -> title,
        "Implementation-Version" -> ver,
        "Implementation-Vendor-Id" -> vendor,
        "Implementation-Vendor" -> vendor
      )
    },
    credentials ++= Seq(
      Credentials(Path.userHome / ".sbt" / ".credentials")
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := { _ =>
      false
    },
    homepage := Some(url("https://github.com/hamnis/openapi-schema")),
    startYear := Some(2021),
    licenses := Seq(
      "Apache2" -> url("https://github.com/hamnis/openapi-schema/blob/master/LICENSE")
    ),
    scmInfo := Some(
      ScmInfo(
        new URL("https://github.com/hamnis/openapi-schema"),
        "scm:git:git@github.com:hamnis/openapi-schema.git",
        Some("scm:git:git@github.com:hamnis/openapi-schema.git")
      )),
    developers ++= List(
      Developer(
        "hamnis",
        "Erlend Hamnaberg",
        "erlend@hamnaberg.net",
        new URL("http://twitter.com/hamnis")
      )
    )
  )
}
