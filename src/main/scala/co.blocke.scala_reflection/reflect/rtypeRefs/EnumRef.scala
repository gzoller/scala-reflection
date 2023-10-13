package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.* 
import rtypes.{ScalaEnumRType, ScalaEnumerationRType, JavaEnumRType}
import util.{JsonField, JsonObjectBuilder}

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
  */
trait EnumRef[R] extends RTypeRef[R]:
  val values: List[String]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", util.Pretty.lastPart(this.getClass.getName)),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("values", this.values)
      )
    )

//---------------------------------------------------------< Scala 3 Enum

case class ScalaEnumRef[R](
    name: String,
    values: List[String]
)(using quotes: Quotes)(using tt: Type[R]) extends EnumRef[R]:
  import quotes.reflect.*
  val typedName: TypedName = name
  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[ScalaEnumRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(values).asTerm
      )
    ).asExprOf[RType[R]]
  
//---------------------------------------------------------< Scala 2 Enumeration

case class ScalaEnumerationRef[R](
    name: String,
    values: List[String]
)(using quotes: Quotes)(using tt: Type[R]) extends EnumRef[R]:
  import quotes.reflect.*
  val typedName: TypedName = name
  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[ScalaEnumerationRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(values).asTerm
      )
    ).asExprOf[RType[R]]


//---------------------------------------------------------< Java Enumeration

// When we get here: we can use class.getEnumConstants() to return array of T, the valid values of a Java enum
case class JavaEnumRef[R](
    name: String,
    values: List[String]
)(using quotes: Quotes)(using tt: Type[R]) extends EnumRef[R]:
  import quotes.reflect.*
  val typedName: TypedName = name
  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[JavaEnumRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(values).asTerm
      )
    ).asExprOf[RType[R]]
