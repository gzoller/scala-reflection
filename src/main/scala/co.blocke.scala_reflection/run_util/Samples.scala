package co.blocke.scala_reflection 


// Scala 2.x style Enumeration
object WeekDayX extends Enumeration {
  type WeekDayX = Value
  val Monday = Value(1)
  val Tuesday = Value(2)
  val Wednesday = Value(99)
  val Thursday = Value(4)
  val Friday = Value(5)
  val Saturday = Value(6)
  val Sunday = Value(-3)
}
import WeekDayX._

// Scala 3 Enum
enum Month {
  case Jan, Feb, Mar
}

case class Birthday(m: Month, d: WeekDayX)