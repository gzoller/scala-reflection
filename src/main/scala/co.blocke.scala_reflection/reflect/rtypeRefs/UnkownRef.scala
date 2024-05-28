package co.blocke.scala_reflection
package reflect
package rtypeRefs

import rtypes.UnknownRType
import scala.quoted.*
import util.{JsonField, JsonObjectBuilder}

case class UnknownRef[R](name: String)(using quotes: Quotes)(using tt: Type[R]) extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName: TypedName = name
  val refType = tt

  val unitVal = '{ null.asInstanceOf[R] }.asExprOf[R]

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[UnknownRType[R]]), "<init>"),
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
        JsonField("rtype", "UnknownRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
