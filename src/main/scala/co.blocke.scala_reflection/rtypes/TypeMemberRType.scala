package co.blocke.scala_reflection
package rtypes

case class TypeMemberRType(
    name: String, // type symbol goes in name
    typeSymbol: Option[TypeSymbol],
    memberType: RType[?]
) extends RType[Any]:

  val typedName = name.asInstanceOf[TypedName]
  override lazy val clazz: Class[?] = Clazzes.AnyClazz
