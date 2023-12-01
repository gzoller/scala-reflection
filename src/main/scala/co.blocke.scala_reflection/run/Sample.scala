package co.blocke.scala_reflection
package run

import neotype.*

trait Miss[E] { val x: E }
case class Foom[X](x: X) extends Miss[X]

case class Person[Y](name: String, again: Option[Person[Y]])

case class Funny[Y](a: Hoovie[Y], b: card, c: Weather, d: Truth, e: Miss[Y])

// Sealed trait (non-enumeration) w/case class
sealed trait Hoovie[X] { val item: X }
case class Cmd[T](item: T) extends Hoovie[T]
case class Quest[T](item: T) extends Hoovie[T]

// Enumeration sealed trait w/case objects
sealed trait card extends Enumeration
case object CLUB extends card
case object HEART extends card
case object DIAMOND extends card
case object SPADE extends card

// Abstract Class w/case objects (Enumeration)
sealed abstract class Weather(val temp: Double)
case object Hot extends Weather(97.0)
case object Cold extends Weather(25.0)

sealed trait Truth
case class Right(isBlessed: Boolean) extends Truth
case class Wrong(excuse: String) extends Truth

case class Mom[R](f: Miss[R])

case class PersonZ[T](msg: Hoovie[T])

// sealed trait Hoovie{ val item: Int}
// case class Cmd(item: Int) extends Hoovie
// case class Quest(item: Int) extends Hoovie

// case class Person(name:String, age:Int)

// type NonEmptyString = NonEmptyString.Type
// given NonEmptyString: Newtype[String] with
//   inline def validate(input: String): Boolean =
//     input.nonEmpty

// type MinorPerson = MinorPerson.Type
// given MinorPerson: Newtype[Person] with
//   inline def validate(input: Person): Boolean =
//     input.age < 18

// case class SampleNeo(name: NonEmptyString, label: String, unknown: MinorPerson)
