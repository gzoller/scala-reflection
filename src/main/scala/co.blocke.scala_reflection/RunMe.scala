package co.blocke.scala_reflection

case class Foom( a: BigInt, b: BigDecimal )
trait Thing[T]{ val name: T }
case class Blather[T](val name: T) //extends Thing[T]
case class Big(i: Int)

case class Item(desc:String)
case class Person(name:String, age:Int, item:Item)

object RunMe extends App:

    println(">> "+RType.of[Person])

//   println(RType.of(Class.forName("co.blocke.scala_reflection.Foom")))
//   println(RType.of(Class.forName("co.blocke.scala_reflection.Blather")))