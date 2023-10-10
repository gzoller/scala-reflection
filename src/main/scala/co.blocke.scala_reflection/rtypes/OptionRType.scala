package co.blocke.scala_reflection
package rtypes

trait OptionRType[R] extends RType[R] with AppliedRType:
  val typeParamSymbols: List[TypeSymbol]
  val optionParamType: RType[?]
  def typeParamValues: List[RType[_]] = List(optionParamType)

//-------------------

case class ScalaOptionRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]:

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"

//-------------------

case class JavaOptionalRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]:

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"
