package co.blocke.scala_reflection
package models

import scala.util.Try

// Type substitution models
//-------------------------
// 0-level
case class DuoTypes[Q,U](a: U, b: Q)  // Note intentional reversal of order to test proper type symbol mapping!

// 1st level type substitution
case class DuoHolder( x: DuoTypes[Int,Float] )

// 2nd level type substitution
case class Thingy[Z](name: String, payload: Z)

// Intersection type substitution
trait Stackable[T]
trait Floatable[U]

// 2nd and 3rd level type substitution - option
case class OptHolder( a: Option[DuoTypes[String,Boolean]])
case class OptHolder2( a: Option[Option[DuoTypes[String,Boolean]]])

// 2nd and 3rd level type substitution - either
case class EitherHolder( a: Either[DuoTypes[Int,Float], Option[DuoTypes[String,Boolean]]])

// 1st and 2nd level substitution in class
case class DuoClass( a: DuoTypes[Int,DuoTypes[Byte,Short]] )

// Try with paramters
trait TryIt[X,Y]{ val x: Try[X]; val y: Try[Option[Y]] }
case class TryItC[A,B]( x: Try[A], y: Try[Option[B]]) extends TryIt[A,B]

// Try substitution
case class TryHolder( a: scala.util.Try[DuoTypes[String,Int]] )

// List and Map substitution
case class ListMapSub( a: List[DuoTypes[Int,Byte]], b: Map[String, DuoTypes[Float,Short]])

// Type member (type symbol) substitution
/*
trait Body
case class FancyBody(message: String) extends Body

case class Envelope[T <: Body, U](id: String, body: T) {
  type Giraffe = T
  type Foo = Int
}
*/