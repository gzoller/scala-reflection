package co.blocke.scala_reflection
package rtypes

case class ObjectRType(
    name: String
) extends RType[Object]:

  val typedName = name
