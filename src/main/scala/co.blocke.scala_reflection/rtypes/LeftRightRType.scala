package co.blocke.scala_reflection
package rtypes

trait LeftRightRType[R] extends AppliedRType:
  self: RType[?] =>

  val leftType: RType[?]
  val rightType: RType[?]
