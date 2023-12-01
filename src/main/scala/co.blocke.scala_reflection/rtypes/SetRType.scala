package co.blocke.scala_reflection
package rtypes

/** Arity 1 Collections, e.g. List, Set, Seq */
case class SetRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?]
) extends RType[R]
    with CollectionRType[R]:

  val typedName: TypedName = name + "[" + elementType.typedName + "]"
