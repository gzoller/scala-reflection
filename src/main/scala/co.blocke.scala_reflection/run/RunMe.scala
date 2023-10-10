package co.blocke.scala_reflection
package run


// case class Person(name: String, age: Int, boss: Option[String])
case class Person(name: String, age: Int, boss: String | Long)

trait Printable[Z]:
  val doc: String
  val numPages: Z


object RunMe extends App:

  println("\n\n")
  val rt = RType.of[Printable[Long]]
  println(rt.pretty)
  println("\n"+rt)

  println("Done.")
