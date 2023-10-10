package co.blocke.scala_reflection
package rtypes

case class UnknownRType[T](name: String) extends RType[T]:
  val typedName = name.asInstanceOf[TypedName]
