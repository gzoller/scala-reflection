import org.typelevel.sbt.gha.JavaSpec.Distribution.Zulu

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
    ),
    Developer(
      "pjfanning",
      "PJ Fanning",
      "",
      url("https://github.com/pjfanning")
    )
  )
))

name := "scala3-reflection"
ThisBuild / organization := "com.github.pjfanning"
ThisBuild / scalaVersion := "3.2.1"

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

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec(Zulu, "8"))
ThisBuild / githubWorkflowOSes := Seq("ubuntu-latest", "windows-latest")
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
  "-encoding",
  "utf8"
)
