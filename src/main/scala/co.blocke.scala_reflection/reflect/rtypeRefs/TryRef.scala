package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.TryRType
import util.{JsonField, JsonObjectBuilder}

case class TryRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    tryRef: RTypeRef[?]
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with AppliedRef:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  val typedName: TypedName = name + "[" + tryRef.typedName + "]"
  val refType = tt

  val unitVal = '{ null.asInstanceOf[R] }.asExprOf[R]

  val selectLimit: Int = 1

  def select(i: Int): RTypeRef[?] =
    if i == 0 then tryRef
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[TryRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typeParamSymbols).asTerm,
        tryRef.expr.asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TryRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("tryType", this.tryRef)
      )
    )

  // candy for ScalaJack
  lazy val hasOptionChild: Option[Language] =
    tryRef match
      case o: ScalaOptionRef[?]  => Some(Language.Scala)
      case o: JavaOptionalRef[?] => Some(Language.Java)
      case lr: LeftRightRef[?]   => lr.hasOptionChild.map(_._2)
      case _                     => None
