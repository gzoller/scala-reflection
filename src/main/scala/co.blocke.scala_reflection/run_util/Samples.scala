package co.blocke.scala_reflection.run_util

case class Foo[T](name:T)

case class Item(desc:String)
case class Person(name:String, age:Int, item:Item)

case class HasDefaults( a: String = "wow", b: Int = 5 )

case class Foom( a: BigInt, b: BigDecimal )
trait Thing[T]{ val name: T }
case class Blather[T](val name: T) //extends Thing[T]
case class Big(i: Int)
