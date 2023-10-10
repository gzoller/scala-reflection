package co.blocke.scala_reflection
package models

// Scala 2.x style Enumeration
object WeekDay extends Enumeration {
  type WeekDay = Value
  val Monday = Value(1)
  val Tuesday = Value(2)
  val Wednesday = Value(99)
  val Thursday = Value(4)
  val Friday = Value(5)
  val Saturday = Value(6)
  val Sunday = Value(-3)
}
import WeekDay.*

// Scala 3 Enum
enum Month {
  case Jan, Feb, Mar
}

case class Birthday(m: Month, d: WeekDay)

enum Color(val rgb: Int) {
  case Red extends Color(0xff0000)
  case Green extends Color(0x00ff00)
  case Blue extends Color(0x0000ff)
  case Mix(mix: Int) extends Color(mix)
}

case class ColorSet(set: Set[Color])
