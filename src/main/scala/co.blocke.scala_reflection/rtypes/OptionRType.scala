package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

trait OptionRType[R] extends RType[R] with AppliedRType:
  val typeParamSymbols: List[TypeSymbol]
  val optionParamType: RType[?]

  def selectLimit: Int = 1
  def select(i: Int): RType[?] =
    if i == 0 then optionParamType
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", Show.lastPart(this.getClass.getName)),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("optionParamType", this.optionParamType)
      )
    )

//-------------------

case class ScalaOptionRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]:

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"

  lazy val clazz: Class[?] = Class.forName(name)

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

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"
  lazy val clazz: Class[?] = Class.forName(name)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val optType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[optionParamType.T] = optionParamType.toType(quotes)
    val optTypeRepr = TypeRepr.of[R](using optType)
    val paramTypeRepr = TypeRepr.of[optionParamType.T](using paramType)
    AppliedType(optTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
