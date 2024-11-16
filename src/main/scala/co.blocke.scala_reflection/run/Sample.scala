package co.blocke.scala_reflection
package model

import neotype.*
import dotty.tools.dotc.reporting.Message

/*
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

type Thing = List[Int]
case class AliasHolder[T](a: T)

type NonEmptyString = NonEmptyString2.Type
object NonEmptyString2 extends Newtype[String]:
  override inline def validate(input: String): Boolean =
    input.nonEmpty

type NonEmptyList = NonEmptyList.Type
given NonEmptyList: Newtype[List[Int]] with
  inline def validate(input: List[Int]): Boolean =
    input.nonEmpty

case class NeoPerson(age: NonEmptyList, desc: NonEmptyString, whatever: Any)

sealed trait Candy
case class Veggies()

type Food = Candy | Veggies
 */
// object Size extends Enumeration {
//   val Small, Medium, Large = Value
// }
// object SizeWithType extends Enumeration {
//   type SizeWithType = Value
//   val Little, Grand = Value
// }
// import SizeWithType.*

// case class SampleEnum(e1: Size.Value, e2: Size.Value, e3: Size.Value, e4: Size.Value, e5: Size.Value, e6: SizeWithType)

/*
object VehicleClass extends Enumeration {
  type VehicleClass = Value
  val Land, Air, Sea = Value
}
import VehicleClass.*

sealed trait Vehicle[K] { val kind: K }
case class Car(passengers: Int) extends Vehicle[Land.type] { val kind: Land.type = Land }

sealed trait Hobby[X, Y] { val thing1: X; val thing2: Y }
sealed trait Artist[W, Z] { val instrument: W; val effort: Z }
sealed trait PersonX[X, Y] { val who: X; val org: Y }

case class Sports[A, B](thing1: A, thing2: B) extends Hobby[A, B]
case class Painter[A, B](instrument: A, effort: B) extends Artist[A, B]
case class Employee[A, B, C, D](who: Artist[C, Hobby[D, A]], org: B) extends PersonX[Artist[C, Hobby[D, A]], B]

case class PP[Z](a:Z)
 */

sealed trait Basic[A, B] {
  val a: A
  val b: B
}

case class Complex(a: Int, b: Boolean) extends Basic[Int, Boolean]
// case class Complex[Y,Z](a:Y, b:Z) extends Basic[Y,Z]

// sealed trait Hobby[X, Y] { val thing1: X; val thing2: Y }
// sealed trait Artist[W, Z] { val instrument: W; val effort: Z }
// sealed trait Person[X, Y] { val who: X; val org: Y }

// case class Sports[A, B](thing1: A, thing2: B) extends Hobby[A, B]
// case class Painter[A, B](instrument: A, effort: B) extends Artist[A, B]
// case class Employee[A, B, C, D](who: Artist[C, Hobby[D, A]], org: B) extends Person[Artist[C, Hobby[D, A]], B]

// // PersonX[String,Int] ==> Employee(Artist)


class Message
class Command extends Message
class System extends Message

case class XMIT(content: List[? <: Message]) // blows up
//case class REC[T <: Message]() // works
