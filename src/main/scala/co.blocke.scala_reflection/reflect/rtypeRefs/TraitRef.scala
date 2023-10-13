package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.TraitRType
import util.{JsonField, JsonObjectBuilder}

case class TraitRef[R](
    name: String,
    typedName: TypedName,
    fields: List[FieldInfoRef],
    typeParamSymbols: List[TypeSymbol] = Nil, // Like T,U
    typeParamValues: List[RTypeRef[_]] = Nil // Like Int, Boolean
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with AppliedRef:
  import quotes.reflect.*
  import Liftables.{ListTypeSymbolToExpr, TypedNameToExpr}

  val refType = tt

  def selectLimit: Int = typeParamSymbols.size
  def select(i: Int): RTypeRef[?] =
    if i >= 0 && i < selectLimit then typeParamValues(i)
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[TraitRType]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typedName).asTerm,
        Expr.ofList(fields.map(_.expr)).asTerm,
        Expr(typeParamSymbols).asTerm,
        Expr.ofList(typeParamValues.map(_.expr)).asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TraitRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("typeParamValues", this.typeParamValues)
      )
    )
