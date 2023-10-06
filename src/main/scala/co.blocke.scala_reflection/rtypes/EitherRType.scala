package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class EitherRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    leftType: RType[?],
    rightType: RType[?]
) extends RType[R]
    with LeftRightRType[R]:

  val typedName: TypedName = name + "[" + leftType.typedName + "," + rightType.typedName + "]"

  lazy val clazz: Class[?] = Class.forName(name)

  def _copy(left: RType[?], right: RType[?]) = EitherRType(name, typeParamSymbols, left, right)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "EitherRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("leftType", this.leftType),
        JsonField("rightType", this.rightType)
      )
    )
