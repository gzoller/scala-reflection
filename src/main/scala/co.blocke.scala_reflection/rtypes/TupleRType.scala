package co.blocke.scala_reflection
package rtypes

case class TupleRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RType[_]]
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name + typeParamValues.map(_.typedName).toList.mkString("[", ",", "]")
