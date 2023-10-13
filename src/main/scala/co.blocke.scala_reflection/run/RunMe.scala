package co.blocke.scala_reflection
package run

object RunMe extends App:

  println("\n\n")

  val rt = RType.inTermsOf[Basis[List[Option[Int | Boolean]]]]("co.blocke.scala_reflection.run.Thingy2")

  // val rt = RType.of[Basis[List[Option[Int | Boolean]]]]

  // val rt = RType.of[Two]

  println(rt)

  println("\n\n" + rt.pretty)

  println("Done.")
