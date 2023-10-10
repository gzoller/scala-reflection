package co.blocke.scala_reflection
package rtypes

trait PrimitiveRType

case class IntRType() extends RType[Int] with PrimitiveRType:
  val name = "scala.Int"
  val typedName: TypedName = name

case class LongRType() extends RType[Long] with PrimitiveRType:
  val name = "scala.Long"
  val typedName: TypedName = name

case class StringRType() extends RType[String] with PrimitiveRType:
  val name = "java.lang.String"
  val typedName: TypedName = name
