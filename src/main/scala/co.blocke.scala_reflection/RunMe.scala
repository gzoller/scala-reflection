package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
// import co.blocke.scala_reflection.run_util.*

object RunMe extends App:


    val rt = RType.inTermsOf[Thing[Boolean]]("co.blocke.scala_reflection.ConcreteThing")

    // val rt = RType.of[Mixed[Int,Boolean]]
    // val rt = RType.of(Class.forName("co.blocke.scala_reflection.NestedOption"))

    println(rt.pretty())

// case class DuoTypes[Q,U](a: U, b: Q)  // Note intentional reversal of order to test proper type symbol mapping!
// case class DuoHolder( a: DuoTypes[Int,Float] )

// co.blocke.scala_reflection.run_util.DuoTypes[scala.Int,scala.Float]