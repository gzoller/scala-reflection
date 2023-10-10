package co.blocke.scala_reflection
package rtypes

case class SealedTraitRType[R](
    name: String,
    children: List[RType[_]]
) extends RType[R]:

  val typedName: TypedName = name + children.map(_.typedName).toList.mkString("[", ",", "]")
