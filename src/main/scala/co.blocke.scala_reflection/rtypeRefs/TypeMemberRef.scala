package co.blocke.scala_reflection
package rtypeRefs

import scala.quoted.*
import rtypes.TypeMemberRType
import util.{JsonField, JsonObjectBuilder}

case class TypeMemberRef(
    name: String,
    memberType: RTypeRef[?]
) (using quotes: Quotes)(using tt: Type[Any]) extends RTypeRef[Any]:
  import quotes.reflect.*

  val typedName: TypedName = name 
  val refType = tt

  val expr =
      Apply(
        Select.unique(New(TypeTree.of[TypeMemberRType]), "<init>"),
        List(
            Expr(name).asTerm,
            memberType.expr.asTerm
        )
      ).asExprOf[RType[Any]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TypeMemberRType"),
        JsonField("typeSymbol", this.name),
        JsonField("memberType", this.memberType)
      )
    )
