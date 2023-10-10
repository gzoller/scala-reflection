package co.blocke.scala_reflection
package rtypes

/** RType for an unassigned type symbol, e.g. Foo[T]
  */

case class TypeSymbolRType[R](name: String) extends RType[R]:
  val typedName = name.asInstanceOf[TypedName]
