package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes


trait OptionRType[R] extends RType[R] with AppliedRType:
  lazy val optionParamType: RType[_]

  def selectLimit: Int = 1
  def select(i: Int): RType[_] = 
    if i == 0 then
      optionParamType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
      

//-------------------


case class ScalaOptionRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _optionParamType: RType[_]
) extends OptionRType[R]:

  val typedName: TypedName = name + "[" + _optionParamType.typedName + "]"

  lazy val clazz: Class[_] = Class.forName(name)
  lazy val optionParamType: RType[_] = _optionParamType

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val optType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[_optionParamType.T] = _optionParamType.toType(quotes)
    val optTypeRepr = TypeRepr.of[R](using optType)
    val paramTypeRepr = TypeRepr.of[_optionParamType.T](using paramType)
    AppliedType(optTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

//-------------------

case class JavaOptionalRType[R](
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _optionParamType: RType[_]
) extends OptionRType[R]:

  val typedName: TypedName = name + "[" + _optionParamType.typedName + "]"
  lazy val clazz: Class[_] = Class.forName(name)
  lazy val optionParamType: RType[_] = _optionParamType

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val optType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[_optionParamType.T] = _optionParamType.toType(quotes)
    val optTypeRepr = TypeRepr.of[R](using optType)
    val paramTypeRepr = TypeRepr.of[_optionParamType.T](using paramType)
    AppliedType(optTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
