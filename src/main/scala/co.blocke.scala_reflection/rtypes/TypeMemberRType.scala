package co.blocke.scala_reflection
package rtypes

import scala.reflect.ClassTag

case class TypeMemberRType(
    name: String, 
    typeSymbol: TypeSymbol, 
    memberType: RType[_],
  ) extends RType[Any]:
  val typedName = name.asInstanceOf[TypedName]
  lazy val clazz: Class[_] = Clazzes.ObjectClazz
