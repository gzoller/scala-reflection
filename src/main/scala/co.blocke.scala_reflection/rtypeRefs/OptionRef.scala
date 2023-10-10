package co.blocke.scala_reflection
package rtypeRefs

import scala.quoted.*
import rtypes.ScalaOptionRType
import util.{JsonField, JsonObjectBuilder}

trait OptionRef[R] extends RTypeRef[R] with AppliedRef:
  val typeParamSymbols: List[TypeSymbol]
  val optionParamType: RTypeRef[?]

  def selectLimit: Int = 1
  def select(i: Int): RTypeRef[?] =
    if i == 0 then optionParamType
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        // JsonField("rtype", Show.lastPart(this.getClass.getName)),
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
)(using quotes: Quotes)(using tt: Type[R]) extends OptionRef[R]:
  import quotes.reflect.*
  import Liftables.ListTypeSymbolToExpr

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"
  val refType = tt

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

/*
case class JavaOptionalRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    optionParamType: RType[?]
) extends OptionRType[R]:

  val typedName: TypedName = name + "[" + optionParamType.typedName + "]"
  lazy val clazz: Class[?] = Class.forName(name)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val optType: quoted.Type[R] =
      super.toType(quotes).asInstanceOf[quoted.Type[R]]
    val paramType: quoted.Type[optionParamType.T] = optionParamType.toType(quotes)
    val optTypeRepr = TypeRepr.of[R](using optType)
    val paramTypeRepr = TypeRepr.of[optionParamType.T](using paramType)
    AppliedType(optTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
*/