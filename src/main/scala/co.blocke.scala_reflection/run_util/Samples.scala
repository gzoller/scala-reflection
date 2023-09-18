package co.blocke.scala_reflection.run_util

case class Foo[T](name:T)

case class Item(desc:String)
case class Person(name:String, age:Int, item:Item)

case class HasDefaults( a: String = "wow", b: Int = 5 )

case class Foom( a: BigInt, b: BigDecimal )
trait Thing[T]{ val name: T }
case class Blather[T](val name: T) //extends Thing[T]
case class Big(i: Int)

case class DuoTypes[Q,U](x: U, y: Q)  // Note intentional reversal of order to test proper type symbol mapping!
case class DuoHolder( a: DuoTypes[Int,Float] )

case class NestedOption(a: Option[Option[Int]], b: String)

case class Mixed(a: (String,Boolean,Person)) 
