package co.blocke.scala_reflection
package rtypes

/** RType for an unassigned type symbol, e.g. Foo[T]
  */

case class TypeSymbolRType(name: String) extends RType[Any]:
  val typedName = name.asInstanceOf[TypedName]
  override lazy val clazz: Class[?] = Clazzes.AnyClazz
