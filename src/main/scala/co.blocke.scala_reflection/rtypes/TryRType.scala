package co.blocke.scala_reflection
package rtypes

case class TryRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    tryType: RType[?]
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name + "[" + tryType.typedName + "]"
  def typeParamValues: List[RType[_]] = List(tryType)