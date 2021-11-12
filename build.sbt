inThisBuild(List(
  organization := "com.github.pjfanning",
  homepage := Some(url("https://github.com/pjfanning/scala3-reflection")),
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

name := "scala3-reflection"
//organization in ThisBuild := "co.blocke"
ThisBuild / organization := "com.github.pjfanning"
scalaVersion := "3.0.2"

ThisBuild / version := "1.0.1-SNAPSHOT"

lazy val root = project
  .in(file("."))
  .settings(settings)
  .settings(
    name := "reflection_library",
    Compile / packageBin / mappings += {
      (baseDirectory.value / "plugin.properties") -> "plugin.properties"
    },
    doc := null,  // disable dottydoc for now
    Compile / doc / sources := Seq(),
    //sources in (Compile, doc) := Seq(),
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler"        % scalaVersion.value,
      "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
      "org.scala-lang" %% "scala3-staging"         % scalaVersion.value,
      "org.scalameta"  %% "munit"                  % "0.7.29" % Test
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
