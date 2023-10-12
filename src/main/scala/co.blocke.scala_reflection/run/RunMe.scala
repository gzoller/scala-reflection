package co.blocke.scala_reflection
package run

object RunMe extends App:

  println("\n\n")

  val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
  val cn = "co.blocke.scala_reflection.run.TFoo6"

  // println(RType.of[T10[T11[Int, T5[Double, Char]], String]].pretty)
  // println("\n\n")
  // println(RType.of(cn).pretty)

  val rt = RType.ito[T10[T11[Int, T5[Double, Char]], String]](cn)
  println(rt.pretty)
  println("\n" + rt)

  // val s = "co.blocke.scala_reflection.run.Foo"
  // println(RType.echo(s))

  // val msg = "Wow!"
  // println(RType.echo(msg))

  println("Done.")
