package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

trait OptionRType[R] extends RType[R] with AppliedRType:
  self: RType[?] =>
  val typeParamSymbols: List[TypeSymbol]
  val optionParamType: RType[?]
  def typeParamValues: List[RType[_]] = List(optionParamType)
  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"

  val selectLimit: Int = 1
  def select(i: Int): RType[?] =
    if i == 0 then optionParamType
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

//-------------------

case class ScalaOptionRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]:

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val optType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[optionParamType.T] = optionParamType.toType(quotes)
    val optTypeRepr = TypeRepr.of[R](using optType)
    val paramTypeRepr = TypeRepr.of[optionParamType.T](using paramType)
    AppliedType(optTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

//-------------------

case class JavaOptionalRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]:

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val optType: quoted.Type[R] =
      super.toType(quotes).asInstanceOf[quoted.Type[R]]
    val paramType: quoted.Type[optionParamType.T] = optionParamType.toType(quotes)
    val optTypeRepr = TypeRepr.of[R](using optType)
    val paramTypeRepr = TypeRepr.of[optionParamType.T](using paramType)
    AppliedType(optTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
