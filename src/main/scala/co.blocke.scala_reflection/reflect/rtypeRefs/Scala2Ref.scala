package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.Scala2RType
import util.{JsonField, JsonObjectBuilder}

/** RType for a Scala 2 class (no Tasty info)
  */
case class Scala2Ref[R](name: String)(using quotes: Quotes)(using tt: Type[R]) extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName: TypedName = name
  val refType = tt

  val unitVal = '{ null }.asExprOf[R]

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[Scala2RType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "Scala2RType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
