// native packager and scoverage are conflicting...
libraryDependencySchemes ++= Seq(
    "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
)
addSbtPlugin("co.blocke" % "gitflow-packager" % "0.1.32")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.9.0")
addSbtPlugin("org.typelevel" % "sbt-typelevel-sonatype-ci-release" % "0.5.0-M6")
addSbtPlugin("org.typelevel" % "sbt-typelevel-github-actions" % "0.7.1")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.12")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.2")