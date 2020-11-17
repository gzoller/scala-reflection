resolvers += Resolver.url(
  "co.blocke release resolver",
  url("https://dl.bintray.com/blocke/releases/")
)(Resolver.ivyStylePatterns)
resolvers += Resolver.url(
  "co.blocke provisional resolver",
  url("https://dl.bintray.com/blocke/provisional/")
)(Resolver.ivyStylePatterns)
addSbtPlugin("co.blocke" % "gitflow-packager" % "0.1.9")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.6")
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.4.6")
