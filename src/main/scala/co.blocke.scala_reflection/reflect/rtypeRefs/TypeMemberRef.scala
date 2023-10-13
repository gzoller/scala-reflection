package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.TypeMemberRType
import util.{JsonField, JsonObjectBuilder}

case class TypeMemberRef(
    name: String,
    typeSymbol: Option[TypeSymbol],
    memberType: RTypeRef[?]
)(using quotes: Quotes)(using tt: Type[Any])
    extends RTypeRef[Any]:
  import quotes.reflect.*
  import Liftables.TypeSymbolToExpr

  val typedName: TypedName = name
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[TypeMemberRType]), "<init>"),
      List(
        Expr(name).asTerm,
        Expr(typeSymbol).asTerm,
        memberType.expr.asTerm
      )
    ).asExprOf[RType[Any]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TypeMemberRType"),
        JsonField("name", this.name),
        JsonField("typeSymbol", this.typeSymbol),
        JsonField("memberType", this.memberType)
      )
    )
