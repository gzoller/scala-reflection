package co.blocke.scala_reflection
package rtypes

/** RType for an unassigned type symbol, e.g. Foo[T]
 */

case class TypeSymbolRType[T](name: String) extends RType[T]:
    val typedName = name.asInstanceOf[TypedName]
    lazy val clazz = Clazzes.ObjectClazz
