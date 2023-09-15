package runnable

import co.blocke.scala_reflection.*

case class Foom( a: BigInt, b: BigDecimal )
trait Thing[T]{ val name: T }
case class Blather[T](val name: T) //extends Thing[T]
case class Big(i: Int)



object RunMe extends App:

    // println(">> "+RType.foo[com.me.Foo[Boolean]])
    println(">> "+RType.of[com.me.Person])
    println(">> "+RType.of[com.me.Person])

//   println(RType.boo[Int])

//   println(RType.of(Class.forName("co.blocke.scala_reflection.Foom")))
//   println(RType.of(Class.forName("co.blocke.scala_reflection.Blather")))