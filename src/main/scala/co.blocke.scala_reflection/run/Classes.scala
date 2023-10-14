package co.blocke.scala_reflection
package run

trait MapIt[X, Y, S, T] { val x: Map[X, Option[Y]]; val s: Array[S]; val t: Array[List[T]] }
case class MapItC[A, B, W, U](x: Map[A, Option[B]], s: Array[W], t: Array[List[U]]) extends MapIt[A, B, W, U]
