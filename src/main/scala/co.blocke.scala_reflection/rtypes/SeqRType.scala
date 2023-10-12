package co.blocke.scala_reflection
package rtypes

/** Arity 1 Collections, e.g. List, Set, Seq */
case class SeqRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?]
) extends RType[R]:

  val typedName: TypedName = name + "[" + elementType.typedName + "]"
