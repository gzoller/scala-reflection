package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
// import co.blocke.scala_reflection.run_util.*
import java.io.*
import co.blocke.scala_reflection.*

object RunMe extends App:

    val rt = RType.of[Birthday]
    println(rt)
    println("\n"+rt.pretty())

    val foo = WeekDayX
    val c = foo.getClass.getName()
    val bar = Class.forName(c)
    println(bar)

    // println(rt2.values)
    // println(rt2.ordinal("Wednesday"))
    // println(rt2.valueAt(2))

