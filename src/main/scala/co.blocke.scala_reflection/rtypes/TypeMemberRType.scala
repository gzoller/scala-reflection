package co.blocke.scala_reflection
package rtypes

case class TypeMemberRType(
    name: String, 
    typeSymbol: TypeSymbol, 
    memberType: RType[_]
  ) extends RType[Any]:
  val typedName = name.asInstanceOf[TypedName]
