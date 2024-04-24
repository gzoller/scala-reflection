

# scala-reflection

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/co.blocke/scala-reflection_3/badge.svg)](https://search.maven.org/artifact/co.blocke/scala-reflection_3/1.1.4/jar)

Scala 2 provided a robust runtime reflection capability that was removed for Scala 3.  Runtime reflection turns out to be quite useful, and is sorely missed.  If you're not already fairly proficient with Scala 3 macros, the new macro-based, compile-time reflection is much more complex to use than the Scala 2 old reflection facility. The scala-reflection library exists to do the hard work of navigating Scala 3 macro reflection and provides simple RType abstractions you can navigate at runtime to see what's inside your classes.  If you are writing your own macros, you can still benefit by using scala-reflections RTypeRef objects and save yourself the headache of handling all the many corner cases of reflection.  (See ScalaJack 8 for examples of using RTypeRef's in your macros.)

## Configuration
In your build.sbt file add the current version of the library to libraryDependences:

```scala
libraryDependencies += "co.blocke" %% "scala-reflection" % CURRENT_VERSION
```
(CURRENT_VERSION value can be taken from the 'maven central' badge in this github repo.)

## Standard Usage
This library defines a set of RType (reflected type) classes, which are high-level abstractions representing information about Scala or Java classes and structures. Reflect on a class like this:

```scala
// File 1
package com.me.models

case class Thing[T](a: String, b: List[T])
```
```scala
// File 2
package com.me.controller

import co.blocke.scala_reflection.*
import com.me.models.*

// >> Compile-time reflection (using square brackets) for types we know at runtime
val macroRType: RType[_] = RType.of[Thing[Int]] // returns ScalaClassRType
println(macroRType.pretty)
/*
com.yourpath.Thing[Int]:
   fields ->
      a: String
      b: List of Int
*/

// >> Run-time reflection using parentheses for type
val runtimeRType: RType[_] = RType.of(className)
```
The second example is when you don't know the actual type of the class to reflect on until runtime.  For example, perhaps the class name is dynamic, passed at runtime.  The only requirement is that the runtime class, given by className, must be somewhere on your JVM class path so that the inspector can find it. 

>If, for any reason, you wish NOT to have scala-reflection examine a class, you may annotate that class with @Ignore and scala-reflection will return UnknownRType.


## Learning to Drive with Macros
scala-reflection uses macros to the fullest extent possible to do the hard work of reflecting on types. Macros impact the compile/test cycle in important ways that are non-intuitive. Take this this example:

```scala
// File1.scala
case  class  Foo(name: String)

// File2.scala
val  fooRType = RType.of[Foo]
```

In Scala 2 (non-macro runtime reflection) if you update Foo in File1.scala you would naturally expect sbt to re-compile this file, and anything else necessary for your changes to be visible wherever used.  sbt does this rather well with no real thought or concern on your part. 

That's **not** necessarily what happens with macros! Remember the macro code is expanded and run at compile-time. In a macro world, if you change Foo in File1.scala and recompile it (alone), you will get a spectacular and exotic exception that will mean very little to you.  What happened?!  The macro, with your old/original Foo definition in it, is still expanded in File2.scala, and there's no mechanism to track this dependency, so RType.of[Foo] will be trying to apply your old expanded Foo macro code against your newly modified Foo class, and BOOM!

Unfortunately there's not (that I have found) a good fix to make sbt aware of this dependency, so you need to ensure both these files are recompiled if Foo changes.  You'll be doing more re-compiling with macros.  It's a macro issue, not a scala-reflection issue.  Since macros are the only access to reflected class information in Scala 3, it's an "ism" we'll all have to live with, until such time as someone invents a way to track macro dependencies in sbt.

## Resolving Generic Classes using Traits
The scala-reflection library was first envisioned to facilitate migrating ScalaJack serialization, which depends heavily on runtime reflection, to Scala 3. One of ScalaJack's key features is its trait handling ability.

```scala
sealed trait  Pet[T] {
  val  name: String
  val  numLegs: Int
  val  special: T
}

case  class  Dog[T](name: String, numLegs: Int, special: T) extends  Pet[T]
case  class  Fish[T](name: String, numLegs: Int, special: T) extends  Pet[T]

val  pet: Pet[Boolean] = Dog("Spot",4,true)
```

When serializing pet, ScalaJack would generate JSON with a type hint like this:
```json
{"_hint":"com.mystuff.Dog","name":"Spot","numLegs":4,"special":true}
```

The hint tells ScalaJack which specific Pet class to materialize upon reading this JSON (we're expecting some Pet). So... you'll see here we just have a class name in the hint. How do we know the type of T?  If we do ```RType.of(Class.forName("com.mystuff.Dog"))``` we get some ScalaClassRType with 'T' as the type of field special. How will we know the type of T?   We're going to have to tell it:

```scala
scalajack.read[Pet[Boolean]](js)
```

Pet[Boolean] is a parameterized trait. We get the class "com.mystuff.Dog" from the JSON, but we will need to resolve Dog ***in terms of*** Pet[Boolean] to find the correct type of 'special'.  (In Scala terminology, we're going to apply the Boolean type to T in the target class Dog.)

We accomplish this feat in scala-reflection like this:

```scala
val  resolved = RType.inTermsOf[Pet[Boolean]](Class.forName("com.mystuff.Dog"))
```

This will return a ScalaClassRType with field 'special' correctly typed to Boolean.


## Scala.js Support (Experimental)
The new 2.x RType objects should be more compatible with Scala.js and you should be able to consume them directly.  The previous version's JSON funcitonality has also been maintained, although some JSON structure may be different.

```scala
  val reflectedThing: String = RType.ofJS[MyThing] 
```
The RType.ofJS macro will create the same RType object as the normal 'of' macro but will serialize it to JSON, which can be parsed and interrogated from within Scala.js.

This feature is experimental, so use as your own risk.  Post issues when you find them.

## Changes from 1.x Versions:
The 2.x series of scala-reflection goes further down the macro rabbit hole for greater performance.

One of the biggest changes for scala-reflection 2.0+ is that all the old "Info" classes are now RType classes, and are typed, for example ScalaCaseClassRType[Person].

> **NOTE:** If you get strange "cyclic dependency errors" this might mean your target class and your call to RType.of[] are in the same compilation unit (same file).  Scala 3 macro reflection doesn't like this.  Simple fix is to not define any classes you reflection within the same file as wherever you call RType.of[].

Pre-2.x versions of scala-reflection provided a compiler plug-in.  Adding a plug into the compiler understandably gave some users pause, and since scala-reflection caches reflected information, keeping the added complexity of the plugin wasn't warranted, and it has been removed in this version.

scala-reflection 2.x now supports parameters in union or intersection types (eg. ```Foo[R,S](a: R|S)```), which was an unsupported feature in 1.x versions.

## Status
At this point the library can reflect on quite a lot of things in the Scala ecosystem:

* Scala 3 Tasty classes (parameterized or non-parameterized) w/annotations
* Traits (including sealed traits)
* Scala 2 case classes
* Value Classes
* Java classes (JavaBeans pattern)
* Scala 3 enum / Scala 2 Enumeration
* Scala 3 Union & Intersection types
* Opaque type aliases
* Try typed fields
* Either
* Option and Java Optional
* Collections, including several Java Collections
* Tuples
* Match/dependent types
* NeoType support (detects NeoTypes) (https://github.com/kitlangton/neotype)

See unit tests for detailed examples of usage.


## Acknowledgements

I wish to thank three people who have helped make this library possible, with their patient explanations and help on forums. Learning Scala 3 reflection internals was certainly a learning curve for me and these guys really helped me through it:

```
Guillaume Martres (@smarter)
Paolo G. Giarrusso (@Blaisorblade)
Nicolas Stucki (@nicolasstucki)

Thanks also goes to pjfanning for multiple contributions to this project.
```

## Release Notes:
* 2.0.4 - Fixes for Option, Scala 2 Enumeration. Move to Scala 3.4.1 and Java 21 LTS
* 2.0.3 - Get rid of some Java deprecation warnings
* 2.0.2 - Re-enable deep reflection of Scala 2 classes
* 2.0.1 - A host of fixes and improvements, eg support of NeoType, and Scala 3.3.3 LTS support
* 2.0.0 - All-new refactor
* 1.2.2 - Scala 3.3.0 LTS support
* 1.2.0 - Experimental Scala.js support
* 1.1.11 - Scala 3.2.1 supported, plus issue fixes
* 1.0.0 - First GA release
* 1.0.0-RC2 - Match compatibility with Scala 3 RC2
* 1.0.0-M2 - Initial release for Scala 3.0.0-M2

