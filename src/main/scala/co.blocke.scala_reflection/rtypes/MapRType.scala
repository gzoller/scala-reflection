package co.blocke.scala_reflection
package rtypes

/** Arity 2 Collections, Map flavors, basiclly */
case class MapRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?], // map key
    elementType2: RType[?] // map value
) extends RType[R]:

  val typedName: TypedName = name + "[" + elementType.typedName + "," + elementType2.typedName + "]"
