package co.blocke.scala_reflection
package rtypes

case class JavaListRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?]
) extends RType[R]
    with CollectionRType[R]:

  val typedName: TypedName = name + "[" + elementType.typedName + "]"
