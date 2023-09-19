package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
// import co.blocke.scala_reflection.run_util.*
import java.io.*

object RunMe extends App:

    println("Enter a class name: ");
    val input = System.console().readLine()
    val rt = RType.inTermsOf(RType.of[Thing[Boolean,Person]],input) //("co.blocke.scala_reflection.ConcreteThing")

    // val rt = RType.of[Mixed[Int,Boolean]]
    // val rt = RType.of(Class.forName("co.blocke.scala_reflection.NestedOption"))

    println(rt.pretty())

// case class DuoTypes[Q,U](a: U, b: Q)  // Note intentional reversal of order to test proper type symbol mapping!
// case class DuoHolder( a: DuoTypes[Int,Float] )

// co.blocke.scala_reflection.run_util.DuoTypes[scala.Int,scala.Float]