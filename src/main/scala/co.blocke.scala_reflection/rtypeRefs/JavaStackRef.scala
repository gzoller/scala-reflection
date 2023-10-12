package co.blocke.scala_reflection
package rtypeRefs

import scala.quoted.*
import rtypes.JavaStackRType
import util.{JsonField, JsonObjectBuilder}

case class JavaStackRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementRef: RTypeRef[?]
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with CollectionRef[R]:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[JavaStackRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typeParamSymbols).asTerm,
        elementRef.expr.asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaStackRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("elementType", this.elementRef)
      )
    )
