package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.TypeSymbolRType
import util.{JsonField, JsonObjectBuilder}

/** RType for an unassigned type symbol, e.g. Foo[T]
  */

case class TypeSymbolRef(name: String)(using quotes: Quotes)(using tt: Type[Any]) extends RTypeRef[Any]:
  import quotes.reflect.*

  val typedName: TypedName = name
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[TypeSymbolRType]), "<init>"),
      List(
        Expr(name).asTerm
      )
    ).asExprOf[RType[Any]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TypeSymbolRType"),
        JsonField("name", this.name)
      )
    )
