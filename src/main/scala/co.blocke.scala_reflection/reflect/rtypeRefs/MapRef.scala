package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.MapRType
import util.{JsonField, JsonObjectBuilder}

/** Arity 2 Collections, Map flavors, basiclly */
case class MapRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementRef: RTypeRef[?], // map key
    elementRef2: RTypeRef[?] // map value
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with CollectionRef[R]:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  override val typedName: TypedName = name + "[" + elementRef.typedName + "," + elementRef2.typedName + "]"

  val refType = tt

  override val selectLimit: Int = 2
  override def select(i: Int): RTypeRef[?] =
    i match {
      case 0 => elementRef
      case 1 => elementRef2
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for $name")
    }

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[MapRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typeParamSymbols).asTerm,
        elementRef.expr.asTerm,
        elementRef2.expr.asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "MapRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("elementType", this.elementRef),
        JsonField("elementType2", this.elementRef2)
      )
    )
