package co.blocke.scala_reflection
package rtypes

case class UnknownRType(name: String) extends RType[Any]:
    val typedName = name.asInstanceOf[TypedName]
    lazy val clazz: Class[_] = Class.forName(name)