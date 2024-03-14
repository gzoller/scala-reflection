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
    typeParamValues: List[RTypeRef[_]] = Nil, // Like Int, Boolean
    sealedChildren: List[RTypeRef[_]] = Nil, // Populated only if this is a sealed class or abstract class
    childrenAreObject: Boolean = false
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with AppliedRef:
  import quotes.reflect.*
  import Liftables.{ListTypeSymbolToExpr, TypedNameToExpr}

  val refType = tt

  val unitVal = '{ null }.asExprOf[R]

  val selectLimit: Int = fields.size
  def select(i: Int): RTypeRef[?] =
    if i >= 0 && i < selectLimit then fields(i).fieldRef
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  def isSealed: Boolean = sealedChildren.nonEmpty

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
        Expr.ofList(typeParamValues.map(_.expr)).asTerm,
        Expr.ofList(sealedChildren.map(_.expr)).asTerm,
        Expr(childrenAreObject).asTerm
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
        JsonField("typeParamValues", this.typeParamValues),
        JsonField("sealedChildren", this.sealedChildren),
        JsonField("childrenAreObject", this.childrenAreObject)
      )
    )
