package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.WildcardRType
import util.{JsonField, JsonObjectBuilder}

/** RType for an unassigned type symbol, e.g. Foo[T]
  */

object WildcardRef:
    def apply(lowBoundsRef: RTypeRef[?], highBoundsRef: RTypeRef[?])(using quotes: Quotes)(using tt: Type[Any]): WildcardRef =
        val low = if lowBoundsRef.name.contains("scala.Nothing") || lowBoundsRef.name.contains("scala.Any") then None else Some(lowBoundsRef)
        val hi = if highBoundsRef.name.contains("scala.Nothing") || highBoundsRef.name.contains("scala.Any") then None else Some(highBoundsRef)
        WildcardRef("?", low, hi)

case class WildcardRef(name: String, lowBoundsRef: Option[RTypeRef[?]], highBoundsRef: Option[RTypeRef[?]])(using quotes: Quotes)(using tt: Type[Any]) extends RTypeRef[Any]:
  import quotes.reflect.*

  val typedName: TypedName = name
  val refType = tt

  val unitVal = '{ "" }

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[WildcardRType]), "<init>"),
      List(
        Expr(name).asTerm,
        ofOption(lowBoundsRef.map(_.expr)).asTerm,
        ofOption(highBoundsRef.map(_.expr)).asTerm
        // if highBoundsRef.isEmpty then '{ None: Option[RTypeRef[?]] } else '{ Some(${highBoundsRef.get.expr}) }
      )
    ).asExprOf[RType[Any]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "WildcardRType"),
        JsonField("name", this.name),
        JsonField("lowBoundsType", this.lowBoundsRef),
        JsonField("highBoundsType", this.highBoundsRef)
      )
    )
