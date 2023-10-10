package co.blocke.scala_reflection
package run

// case class Person(name: String, age: Int, boss: Option[String])
case class Person(name: String, age: Int, boss: String | Long)

trait Billable 

trait Printable[Z]:
  val doc: String
  val numPages: Z

class Foo(bar: Int)

case class Flyer[F](doc: String, numPages: F) extends Printable[F]