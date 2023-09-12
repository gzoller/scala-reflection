package co.blocke.scala_reflection
package rtypes

case class TypeMemberRType[R](
    name: String, 
    typeSymbol: TypeSymbol, 
    memberType: RType[R]
  ) extends RType[R]
