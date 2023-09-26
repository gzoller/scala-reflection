package co.blocke.scala_reflection
package rtypes

/** RType for a Scala 2 class (no Tasty info)
 */
case class Scala2RType[R](name: String) extends RType[R]:
  val typedName: TypedName = name
  lazy val clazz: Class[_] = Class.forName(name)
