package co.blocke.scala_reflection
package rtypes

import Clazzes.*

trait PrimitiveRType

case class IntRType() extends RType[Int] with PrimitiveRType:
  val name = INT_CLASS
  val typedName: TypedName = name

case class LongRType() extends RType[Long] with PrimitiveRType:
  val name = LONG_CLASS
  val typedName: TypedName = name

case class StringRType() extends RType[String] with PrimitiveRType:
  val name = STRING_CLASS
  val typedName: TypedName = name

case class AnyRType() extends RType[Any] with PrimitiveRType:
  val name = ANY_CLASS
  val typedName: TypedName = name
