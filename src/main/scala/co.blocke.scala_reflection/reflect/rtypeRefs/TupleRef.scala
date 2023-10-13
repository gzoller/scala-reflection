package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.TupleRType
import util.{JsonField, JsonObjectBuilder}

case class TupleRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    tupleRefs: List[RTypeRef[_]]
)(using quotes: Quotes)(using tt: Type[R]) extends RTypeRef[R]
    with AppliedRef:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  val typedName: TypedName = name + tupleRefs.map(_.typedName).toList.mkString("[", ",", "]")
  val refType = tt

  def selectLimit: Int = tupleRefs.size

  def select(i: Int): RTypeRef[?] =
    if i >= 0 && i <= tupleRefs.size - 1 then tupleRefs(i)
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[TupleRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typeParamSymbols).asTerm,
        Expr.ofList( tupleRefs.map(_.expr) ).asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TupleRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("tupleTypes", this.tupleRefs)
      )
    )
