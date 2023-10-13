package co.blocke.scala_reflection
package run

trait Basis[T] {
  val a: Int
  val b: String
  val c: T
}
case class Thingy2[T](a: Int, b: String, c: T) extends Basis[T]

case class One(a: List[String])
case class Two(b: Option[String])
