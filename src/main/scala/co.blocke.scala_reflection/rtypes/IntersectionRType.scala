package co.blocke.scala_reflection
package rtypes

case class IntersectionRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    leftType: RType[?],
    rightType: RType[?]
) extends RType[R]
    with LeftRightRType:

  val typedName: TypedName = name + "[" + leftType.typedName + "," + rightType.typedName + "]"
  def typeParamValues: List[RType[_]] = List(leftType, rightType)
