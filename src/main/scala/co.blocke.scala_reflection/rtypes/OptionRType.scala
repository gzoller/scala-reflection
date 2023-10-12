package co.blocke.scala_reflection
package rtypes

trait OptionRType[R] extends RType[R] with AppliedRType:
  self: RType[?] =>
  val typeParamSymbols: List[TypeSymbol]
  val optionParamType: RType[?]
  def typeParamValues: List[RType[_]] = List(optionParamType)
  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"

//-------------------

case class ScalaOptionRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]

//-------------------

case class JavaOptionalRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]
