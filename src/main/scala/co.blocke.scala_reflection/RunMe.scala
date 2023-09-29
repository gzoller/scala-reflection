package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
// import co.blocke.scala_reflection.run_util.*
import java.io.*
import co.blocke.scala_reflection.*

object RunMe extends App:

    val rt = RType.of[Foo[Boolean]]
    println(rt.pretty)
