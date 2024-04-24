package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.{JavaOptionalRType, ScalaOptionRType}
import util.{JsonField, JsonObjectBuilder}

trait OptionRef[R] extends RTypeRef[R] with AppliedRef:
  val typeParamSymbols: List[TypeSymbol]
  val optionParamType: RTypeRef[?]

  val selectLimit: Int = 1
  def select(i: Int): RTypeRef[?] =
    if i == 0 then optionParamType
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", util.Pretty.lastPart(this.getClass.getName).replace("Ref", "RType")),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("optionParamType", this.optionParamType)
      )
    )

//-------------------

case class ScalaOptionRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RTypeRef[?]
)(using quotes: Quotes)(using tt: Type[R])
    extends OptionRef[R]:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"
  val refType = tt

  val unitVal = '{ None }.asExprOf[R]

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[ScalaOptionRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typeParamSymbols).asTerm,
        optionParamType.expr.asTerm
      )
    ).asExprOf[RType[R]]

//-------------------

case class JavaOptionalRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RTypeRef[?]
)(using quotes: Quotes)(using tt: Type[R])
    extends OptionRef[R]:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"
  val refType = tt

  val unitVal = '{ java.util.Optional.empty().asInstanceOf[R] }.asExprOf[R]

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[JavaOptionalRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typeParamSymbols).asTerm,
        optionParamType.expr.asTerm
      )
    ).asExprOf[RType[R]]
