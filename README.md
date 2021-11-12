# Scala3 Reflection

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.pjfanning/scala3-reflection_3/badge.svg)](https://search.maven.org/artifact/com.github.pjfanning/scala3-reflection_3/1.0.1/jar)

This is a fork of [gzoller/scala-reflection](https://github.com/gzoller/scala-reflection).
Hopefully, this fork will only be needed as a short term measure. The fixes in this fork have been
submitted as Pull Requests to the upstream project.

This fork is used by [jackson-scala3-reflection-extensions](https://github.com/pjfanning/jackson-scala3-reflection-extensions).

Please read the readme on [gzoller/scala-reflection](https://github.com/gzoller/scala-reflection) to get a better
idea how this project works.

## Configuration

```scala
libraryDependencies += "com.github.pjfanning" %% "scala3-reflection" % "1.0.1"
```

For best results, compile all classes you intend to reflect on with this plugin enabled.

```scala
addCompilerPlugin("co.blocke" %% "scala-reflection" % "1.0.0")
```

## Release Notes:

* 1.0.1 - First fork release
* 1.0.0 - Most recent [gzoller/scala-reflection](https://github.com/gzoller/scala-reflection) release
