package co.blocke.scala_reflection
package run

object RunMe extends App:

  println("\n\n")

  val rt = RType.inTermsOf[MapIt[Int, Double, String, Boolean]]("co.blocke.scala_reflection.run.MapItC")

  // println(rt)

  println("\n\n" + rt.pretty)

  println("Done.")

  /*

trait MapIt[X, Y, S, T] { val x: Map[X, Option[Y]]; val s: Array[S]; val t: Array[List[T]] }
case class MapItC[A, B, W, U](x: Map[A, Option[B]], s: Array[W], t: Array[List[U]]) extends MapIt[A, B, W, U]

   */
