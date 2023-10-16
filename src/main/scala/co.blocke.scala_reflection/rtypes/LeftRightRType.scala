package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

trait LeftRightRType[R] extends AppliedRType:
  self: RType[?] =>

  val leftType: RType[?]
  val rightType: RType[?]
