package co.blocke.scala_reflection
package run

object RunMe extends App:

  println("\n\n")
  // val rt = RType.of("co.blocke.scala_reflection.run.Flyer")
  val rt = RType.inTermsOf[Printable[Int]]("co.blocke.scala_reflection.run.Flyer")
  println(rt.pretty)
  println("\n" + rt)

  println("Done.")
