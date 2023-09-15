
# scala-reflection

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/co.blocke/scala-reflection_3/badge.svg)](https://search.maven.org/artifact/co.blocke/scala-reflection_3/1.1.4/jar)

The scala-reflection project seeks to replace the lost runtime reflection capability that Scala 2 had, but that was removed in Scala 3.  Those who have followed this project know that it accomplishes this by some cleaver use of Scala 3 macros--basically we dug around in the compiler internals, so you won't have to!

This latest version of scala-reflection goes even further down the macro rabbit hole for greater performance, and we eliminated the need for the previous versions' compiler plug-in.

One of the biggest changes for scala-reflection 2.0+ is that all the old "Info" classes are now RType classes, and are typed, for example ScalaCaseClassRType[Person].

> **NOTE:** One interesting oddment from Scala macros is that your target class (the one you want an RType for) <u>must not be in the same package</u> as wherever you run RType.of[].  If you do, you will get cyclic dependency errors.  Not sure why--Scala internals? If you do get that error, just keep your target classes in separate packages from your controller code and you'll be fine--and it's a good practice anyway. 

The scala-reflection project seeks to accomplish two goals:

* Make Scala 3 reflection a more approachable by exposing higher-level abstractions for reflected things, vs using macros to dive through Scala 3 internals

* Allow for a true, performant, runtime reflection capability

That second goal, runtime reflection, poses a unique challenge. Just how do you provide a runtime reflection ability in a language that doesn't have that facility? How, indeed! 

>Pre-2.0.0 versions of scala-reflection provided a compiler plug-in.  Adding a plug into the compiler understandably gave some users pause, and since scala-reflection caches reflected information, keeping the added complexity of the plugin wasn't warranted.

## Configuration
In your build.sbt file be sure you've set co.blocke's releases repo in bintray as a resolver and add the current version of the library to libraryDependences:

```scala
libraryDependencies += "co.blocke" %% "scala-reflection" % CURRENT_VERSION
```

(CURRENT_VERSION value can be taken from the 'maven central' badge in this github repo.)


## Standard Usage
This library defines a set of RType (reflected type) classes, which are high-level abstractions representing reflected information about various Scala classes and structures. Reflect on a class like this:

```scala
// Note package for target (model) classes is different than package for macro running!
package com.me.models

case class Thing(a: String)
```
```scala
package com.me.controller // <-- different package from target classes!
import co.blocke.scala_reflection.*
import com.me.models.*

// >> Compile-time reflection using square brackets for type
val macroRType: RType = RType.of[Thing] // returns ScalaCaseClassRType

// >> Run-time reflection using parentheses for type
val className: String = getClassWeNeedFromSomewhere()
val runtimeRType: RType = RType.of(Class.forName(className))
```
The second example is for when you don't know the actual type of the class to reflect on until runtime.  The first time scala-reflection sees a particular class, file IO will read your class' .tasty file and reflect on the class, which is slow, but this information will be cached so subsequent accesses will be extremely fast--as fast as a cache lookup.

## Resolving Generic Classes using Traits
The scala-reflection library was first envisioned to facilitate migrating ScalaJack serialization, which depends heavily on runtime reflection, to Scala 3. One of ScalaJack's key features is its trait handling ability.

```scala
trait  Pet[T] {
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

The hint tells ScalaJack which specific Pet class to materialize upon reading this JSON (we're expecting only a Pet). So... you'll see here we just have a class name in the hint. How do we know the type of T? We're going to have to tell it:

```scala
scalajack.read[Pet[Boolean]](js)
```

Pet[Boolean] is a parameterized trait. We get the concrete class value "com.mystuff.Dog" from the JSON. We need to resolve Dog ***in terms of*** Pet[Boolean] to find the correct type of 'special'.

We accomplish this feat in scala-reflection like this:

```scala
val  resolved = RType.inTermsOf[Pet[Boolean]](Class.forName("com.mystuff.Dog"))
```

This will return a ScalaCaseClassRType with field 'special' correctly typed to Boolean, which it learned about by studying the specific Pet trait you gave it in the square brackets.

Here's what the process looks like internally:
##### RType.of[Pet[Boolean]:
```scala
TraitInfo(com.foo.Pet) actualParamTypes: [
   T: scala.Boolean
] with  fields:
   name: java.lang.String
   numLegs: scala.Int
   special[T]: scala.Boolean
```
##### RType.of(Class.forName("com.foo.Dog"):
```scala
ScalaCaseClassInfo(com.foo.Dog):
   fields:
      (0) name: java.lang.String
      (1) numLegs: scala.Int
      (2)[T] special: scala.Any
```

##### RType.inTermsOf[Pet[Boolean]](Class.forName("com.foo.Dog")
```scala
ScalaCaseClassRType(com.foo.Dog):
   fields:
      (0) name: java.lang.String
      (1) numLegs: scala.Int
      (2)[T] special: scala.Boolean
```

>If, for any reason, you wish NOT to have scala-reflection examine a class, you may annotate that class with @Skip_Reflection and scala-reflection will return UnknownRType.

## Learning to Drive with Macros
scala-reflection uses macros to the fullest extent possible to do the hard work of reflecting on types. Macros impact the compile/test cycle in ways that are non-intuitive. Think of this example:

```scala
// File1.scala
case  class  Foo(name: String)

// File2.scala
val  fooRType = RType.of[Foo]
```

In Scala 2 (non-macro runtime reflection) if you update Foo in File1.scala you would naturally expect sbt to re-compile this file, and anything that depends on Foo, and the changes will be picked up in your program, and all will be well, with no real thought or concern on your part. 

That's **not** necessarily what happens with macros! Remember the macro code is run at compile-time. File2.scala needs to be re-compiled because the RType.of macro needs to be re-expanded to pick up your changes to Foo class in File1.scala. *Unfortunately sbt doesn't pick up this dependency!* If you don't know any better you'll just re-compile and re-run your program after a change to File1.scala, as usual, and  you'll get a **spectacular exception with exotic errors** that won't mean much to you. The solution is you need to also recompile File2.scala.

This means you will be doing more re-compiling with macro-based code than you would without the macros. It's an unfortunate cost of inconvenience and time, but the payoff is a *dramatic* gain in speed at runtime, and in the case of reflection in Scala 3, using macros is really the only way to accomplish reflection.

## Scala.js Support (Experimental)
It would be great if you could reflect on Scala classes from within scala.js, right?  Unfortunately, scala.js doesn't play well with the RType objects generated by scala-reflection.  To address this problem a new reflection macro has been created:

```scala
  val reflectedThing: String = RType.ofJS[MyThing] 
```
The RType.ofJS macro will create the same RType object as the normal 'of' macro but will serialize it to JSON, which can be parsed and interrogated from within Scala.js.

This feature is experimental, so use as your own risk.  Post issues when you find them.


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
* Collections, incl. several Java Collections
* Tuples

See unit tests for detailed examples of usage.

## Limitations

* No support for parameters in Intersection or Union types (```val t: X|Y``` or ```val u: X&Y```). This is because union/intersection types don't appear to be implemented as full classes in Scala and we haven't yet figured out this would work in scala-reflection.


## Acknowledgements

I wish to thank three people who have helped make this library possible, with their patient explanations and help on gitter. Learning Scala 3 reflection internals was certainly a learning curve for me and these guys really helped me through it:

```
Guillaume Martres (@smarter)
Paolo G. Giarrusso (@Blaisorblade)
Nicolas Stucki (@nicolasstucki)
```

## Release Notes:

* 1.2.2 - Scala 3.3.0 LTS support
* 1.2.0 - Experimental Scala.js support
* 1.1.11 - Scala 3.2.1 supported, plus issue fixes
* 1.0.0 - First GA release
* 1.0.0-RC2 - Match compatibility with Scala 3 RC2
* 1.0.0-M2 - Initial release for Scala 3.0.0-M2

