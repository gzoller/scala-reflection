package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.ObjectRType
import util.{JsonField, JsonObjectBuilder}

case class ObjectRef[R](
    name: String
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName = name
  val refType = tt

  val unitVal = Ref(TypeRepr.of[R].typeSymbol).asExprOf[R] // get object instance

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[ObjectRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ObjectRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
