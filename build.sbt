inThisBuild(List(
  organization := "co.blocke",
  homepage := Some(url("https://github.com/gzoller/scala-reflection")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "gzoller",
      "Greg Zoller",
      "gzoller@outlook.com",
      url("http://www.blocke.co")
    )
  )
))

name := "scala-reflection"
organization in ThisBuild := "co.blocke"
scalaVersion := "3.0.0-RC3"

lazy val root = project
  .in(file("."))
  .settings(settings)
  .settings(
    name := "reflection_library",
    Compile / packageBin / mappings += {
      (baseDirectory.value / "plugin.properties") -> "plugin.properties"
    },
    doc := null,  // disable dottydoc for now
    sources in (Compile, doc) := Seq(),
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler"        % scalaVersion.value,
      "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
      "org.scala-lang" %% "scala3-staging"         % scalaVersion.value,
      "org.scalameta"  %% "munit"                  % "0.7.25" % Test
    )
  )

//==========================
// Settings
//==========================
lazy val settings = Seq(
  scalacOptions ++= compilerOptions,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-encoding",
  "utf8"
)
