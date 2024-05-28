package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.IterableRType
import util.{JsonField, JsonObjectBuilder}

/** Arity 1 Collections, e.g. List, Set, Seq */
case class IterableRef[R <: scala.collection.Iterable[?]](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementRef: RTypeRef[?]
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with CollectionRef[R]:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  val refType = tt
  val isMutable = name.contains(".mutable.")

  val unitVal = '{ null.asInstanceOf[R] }.asExprOf[R]

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[IterableRType[R]]), "<init>"),
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
        JsonField("rtype", "IterableRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("elementType", this.elementRef)
      )
    )
