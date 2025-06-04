package co.blocke.scala_reflection
package rtypes

case class ArrayRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?]
) extends RType[R]
    with CollectionRType[R]:

  val typedName = name + "[" + elementType.typedName + "]"
