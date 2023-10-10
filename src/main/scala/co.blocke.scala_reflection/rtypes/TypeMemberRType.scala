package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

case class TypeMemberRType(
    name: String, // type symbol goes in name
    memberType: RType[?]
) extends RType[Any]:

  val typedName = name.asInstanceOf[TypedName]
