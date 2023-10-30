package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.{EitherRType, IntersectionRType, UnionRType}
import util.{JsonField, JsonObjectBuilder}

enum LRKind(name: String) {
  case EITHER extends LRKind("EitherRType")
  case INTERSECTION extends LRKind("IntersectionRType")
  case UNION extends LRKind("UnionRType")
  override def toString(): String = this.name
}

given LRKindToExpr: ToExpr[LRKind] with {
  def apply(x: LRKind)(using Quotes): Expr[LRKind] =
    x match
      case LRKind.EITHER       => '{ LRKind.EITHER }
      case LRKind.INTERSECTION => '{ LRKind.INTERSECTION }
      case LRKind.UNION        => '{ LRKind.UNION }
}

/** Marker trait for all Scala/Java left/right types (either, intersection, union) */
case class LeftRightRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    leftRef: RTypeRef[?],
    rightRef: RTypeRef[?],
    lrkind: LRKind
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with AppliedRef:
  import quotes.reflect.*
  import Liftables.{ListTypeSymbolToExpr, TypedNameToExpr}

  val typedName: TypedName = name + "[" + leftRef.typedName + "," + rightRef.typedName + "]"
  val refType = tt

  val selectLimit: Int = 2

  def select(i: Int): RTypeRef[?] =
    i match {
      case 0 => leftRef
      case 1 => rightRef
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for $name")
    }

  type LRType[X] = X match
    case scala.util.Either[?, ?] => EitherRType[R]
    case IntersectionType        => IntersectionRType[R]
    case UnionType               => UnionRType[R]

  private val reprMap = Map(
    LRKind.EITHER -> { () => TypeRepr.typeConstructorOf(classOf[EitherRType[R]]) },
    LRKind.INTERSECTION -> { () => TypeRepr.typeConstructorOf(classOf[IntersectionRType[R]]) },
    LRKind.UNION -> { () => TypeRepr.typeConstructorOf(classOf[UnionRType[R]]) }
  )

  val expr =
    implicit val q = quotes
    val pt = reprMap(lrkind)().asType.asInstanceOf[Type[LRType[R]]]
    tt match
      case '[t] =>
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[LRType[R]](using pt)), "<init>"),
            List(TypeTree.of[R])
          ),
          List(
            Expr(name).asTerm,
            Expr(typeParamSymbols).asTerm,
            leftRef.expr.asTerm,
            rightRef.expr.asTerm
          )
        ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", lrkind.toString),
        JsonField("name", name),
        JsonField("typedName", typedName),
        JsonField("typeParamSymbols", typeParamSymbols),
        JsonField("leftType", leftRef),
        JsonField("rightType", rightRef)
      )
    )
