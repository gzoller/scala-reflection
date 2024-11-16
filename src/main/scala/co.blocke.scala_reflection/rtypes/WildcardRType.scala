package co.blocke.scala_reflection
package rtypes

/** RType for an bounded wildcard type symbol, e.g. [? <: Thing]
  */

case class WildcardRType(name: String, lowBoundsType: Option[RType[?]], highBoundsType: Option[RType[?]]) extends RType[Any]:
  val typedName = name.asInstanceOf[TypedName]
  override lazy val clazz: Class[?] = Clazzes.AnyClazz
