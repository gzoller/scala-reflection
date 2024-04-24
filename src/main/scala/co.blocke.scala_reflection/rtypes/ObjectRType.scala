package co.blocke.scala_reflection
package rtypes

case class ObjectRType[R](
    name: String
) extends RType[R]:

  val typedName = name
