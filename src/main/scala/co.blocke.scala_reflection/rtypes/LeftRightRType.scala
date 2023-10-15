package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

trait LeftRightRType[R] extends AppliedRType:
  self: RType[?] =>

  val leftType: RType[?]
  val rightType: RType[?]

  val selectLimit: Int = 2

  def select(i: Int): RType[?] =
    i match {
      case 0 => leftType
      case 1 => rightType
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for $name")
    }
