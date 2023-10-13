package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.SealedTraitRType
import util.{JsonField, JsonObjectBuilder}

case class SealedTraitRef[R](
    name: String,
    children: List[RTypeRef[_]]
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName: TypedName = name + children.map(_.typedName).toList.mkString("[", ",", "]")
  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[SealedTraitRType]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr.ofList(children.map(_.expr)).asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "SealedTraitRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("children", this.children)
      )
    )
