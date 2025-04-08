import org.typelevel.sbt.gha.JavaSpec.Distribution.Zulu
import scoverage.ScoverageKeys._

lazy val isCI = sys.env.get("CI").contains("true")

inThisBuild(List(
  organization := "co.blocke",
  homepage := Some(url("https://github.com/gzoller/scala-reflection")),
  licenses := List("MIT" -> url("https://opensource.org/licenses/MIT")),
  developers := List(
    Developer(
      "gzoller",
      "Greg Zoller",
      "gzoller@blocke.co",
      url("http://www.blocke.co")
    ),
    Developer(
      "pjfanning",
      "PJ Fanning",
      "",
      url("https://github.com/pjfanning")
    )
  )
))

name := "scala-reflection"
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / organization := "co.blocke"
ThisBuild / scalaVersion := "3.5.2"
ThisBuild / githubWorkflowScalaVersions := Seq("3.5.2")

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
    scalafmtOnCompile := !isCI,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler"        % scalaVersion.value,
      "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
      "org.scala-lang" %% "scala3-staging"         % scalaVersion.value,
      "io.github.kitlangton" %% "neotype"          % "0.2.4",
      "org.scalameta"  %% "munit"                  % "1.0.0-M9" % Test,
      "co.blocke"      %% "listzipper"             % "0.1.6" % Test
    )
  )

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Zulu, "21"))
ThisBuild / githubWorkflowOSes := Seq("ubuntu-20.04", "windows-latest")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(
  RefPredicate.Equals(Ref.Branch("main")),
  RefPredicate.StartsWith(Ref.Tag("v"))
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
      "CI_SNAPSHOT_RELEASE" -> "+publishSigned"
    )
  )
)

//==========================
// Settings
//==========================
lazy val settings = Seq(
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= compilerOptions,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  // "-explain",
  // "-experimental",
  "-encoding",
  "utf8"
)
