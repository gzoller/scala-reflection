package co.blocke.scala_reflection
package rtypes

case class TraitRType[R](
    name: String,
    typedName: TypedName,
    fields: List[FieldInfo],
    typeParamSymbols: List[TypeSymbol] = Nil, // Like T,U
    typeParamValues: List[RType[_]] = Nil // Like Int, Boolean
) extends RType[R]
    with AppliedRType
