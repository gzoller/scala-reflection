package co.blocke.scala_reflection
package rtypes

import scala.util.Try
import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class TryRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    tryType: RType[?]
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name + "[" + tryType.typedName + "]"
  def selectLimit: Int = 1
  lazy val clazz: Class[?] = Class.forName(name)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val thisTryType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[tryType.T] = tryType.toType(quotes)
    val tryTypeRepr = TypeRepr.of[R](using thisTryType)
    val paramTypeRepr = TypeRepr.of[tryType.T](using paramType)
    AppliedType(tryTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

  def select(i: Int): RType[?] =
    if i == 0 then tryType
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TryRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("tryType", this.tryType)
      )
    )
