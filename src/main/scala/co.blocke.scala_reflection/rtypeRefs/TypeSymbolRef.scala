package co.blocke.scala_reflection
package rtypeRefs

import scala.quoted.*
import rtypes.TypeSymbolRType
import util.{JsonField, JsonObjectBuilder}

/** RType for an unassigned type symbol, e.g. Foo[T]
  */

case class TypeSymbolRef[R](name: String)(using quotes: Quotes)(using tt: Type[R]) extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName: TypedName = name 
  val refType = tt

  val expr =
      Apply(
        Select.unique(New(TypeTree.of[TypeSymbolRType]), "<init>"),
        List(
            Expr(name).asTerm
        )
      ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TypeSymbolRType"),
        JsonField("name", this.name)
      )
    )
