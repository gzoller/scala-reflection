name := "scala-reflection"
organization in ThisBuild := "co.blocke"
val dottyVersion =  "0.28.0-bin-SNAPSHOT" //"0.27.0-RC1"

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
    libraryDependencies ++= commonDependencies
  )

//==========================
// Dependencies
//==========================
lazy val dependencies =
  new {
    val dottyCompiler = "ch.epfl.lamp" %% "dotty-compiler" % dottyVersion
    val dottyInspection = "ch.epfl.lamp" %% "dotty-tasty-inspector" % dottyVersion
    val munit = "org.scalameta" %% "munit" % "0.7.12+51-8feb6e8b-SNAPSHOT" % Test
  }

lazy val commonDependencies = Seq(
  dependencies.dottyCompiler,
  dependencies.dottyInspection,
  dependencies.munit
)

//==========================
// Settings
//==========================
lazy val settings = 
  commonSettings ++
  jacocoSettings ++
  publishSettings

lazy val compilerOptions = Seq(
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-deprecation",
  "-encoding",
  "utf8"
)

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers += Resolver.jcenterRepo,
  scalaVersion := dottyVersion,
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  bintrayOrganization := Some("blocke"),
  bintrayReleaseOnPublish in ThisBuild := true,
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  bintrayRepository := "releases",
  bintrayPackageLabels := Seq("scala", "dotty", "reflection")
)

lazy val jacocoSettings = Seq(
)
