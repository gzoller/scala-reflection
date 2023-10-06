package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class UnionRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    leftType: RType[?],
    rightType: RType[?]
) extends RType[R]
    with LeftRightRType[R]:

  val typedName: TypedName = name + "[" + leftType.typedName + "," + rightType.typedName + "]"

  lazy val clazz: Class[?] = Clazzes.AnyClazz // something here so we don't blow up

  def _copy(left: RType[?], right: RType[?]) = this.copy(leftType = left, rightType = right)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "UnionRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("leftType", this.leftType),
        JsonField("rightType", this.rightType)
      )
    )
