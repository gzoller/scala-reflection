package runnable

import co.blocke.scala_reflection.*
import co.blocke.scala_reflection.rtypes.*
// import co.blocke.scala_reflection.run_util.*
import java.io.*
import co.blocke.scala_reflection.*

object RunMe extends App:

  println("\n\n")
  val rt = RType.ofJS[Person[Bogus]]
  println(rt)

  println("\n\n")
  println(RType.of[Person[Long]].pretty)

  println("Done.")
