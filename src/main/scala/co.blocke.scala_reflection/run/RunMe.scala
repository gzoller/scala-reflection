package co.blocke.scala_reflection
package run

object RunMe extends App:

  println("\n\n")

  // val rt = RType.mapTypesForSymbols[MapIt[Int, Double, String, Boolean]]("co.blocke.scala_reflection.run.MapItC")

  println("--1--")
  // val cname = "co.blocke.scala_reflection.run.MapItC"
  print("Enter a classname: ")
  val cname = scala.io.StdIn.readLine()
  val rt = RType.of(cname)

  // println("--2--")
  // RType.of("co.blocke.scala_reflection.run.MapItC")

  // println(rt)

  println("\n\n" + rt.pretty)

  // print("Enter you name: ")
  // val name = scala.io.StdIn.readLine()
  // println(RType.echo(name))

  println("Done.")

  /*

trait MapIt[X, Y, S, T] { val x: Map[X, Option[Y]]; val s: Array[S]; val t: Array[List[T]] }
case class MapItC[A, B, W, U](x: Map[A, Option[B]], s: Array[W], t: Array[List[U]]) extends MapIt[A, B, W, U]

   */
