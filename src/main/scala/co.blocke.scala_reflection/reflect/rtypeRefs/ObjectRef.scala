package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.ObjectRType
import util.{JsonField, JsonObjectBuilder}

case class ObjectRef(
    name: String
)(using quotes: Quotes)(using tt: Type[Object])
    extends RTypeRef[Object]:
  import quotes.reflect.*

  val typedName = name
  val refType = tt

  val unitVal = '{ null }.asExprOf[Object]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[ObjectRType]), "<init>"),
      List(
        Expr(name).asTerm
      )
    ).asExprOf[RType[Object]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ObjectRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
