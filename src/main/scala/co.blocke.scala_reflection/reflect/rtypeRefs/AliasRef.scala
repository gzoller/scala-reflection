package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.AliasRType
import util.{JsonField, JsonObjectBuilder}

case class AliasRef[R](
    definedType: String,
    unwrappedType: RTypeRef[?] // Aliases with a parameterized wrapped type are not currently supported, so ConcreteType here.
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*

  val name: String = definedType.drop(definedType.lastIndexOf('.') + 1)
  val typedName: TypedName = name
  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[AliasRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(definedType).asTerm,
        unwrappedType.expr.asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "AliasRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("definedType", this.definedType),
        JsonField("unwrappedType", this.unwrappedType)
      )
    )
