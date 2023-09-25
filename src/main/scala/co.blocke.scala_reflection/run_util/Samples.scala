package co.blocke.scala_reflection 


trait Basis[Z] {
  val a: Int
  val b: String
  val c: Z
}
case class Thingy2[T]( a: Int, b: String, c: T) extends Basis[T]