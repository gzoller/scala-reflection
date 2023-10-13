package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.SelfRefRType
import util.{JsonField, JsonObjectBuilder}

/** Placeholder RType to be lazy-resolved, used for self-referencing types.  This is needed because without it, reflecting on
  *  a self-referencing type will enter an endless loop until the stack explodes.  This RType is immediately inserted into the
  *  type cache so that when the self-reference comes there's something in the cache to find.
  *  When one of these is encountered in the wild, just re-Reflect on the infoClass and you'll get the non-SelfRef (i.e. normal) RType
  */
case class SelfRefRef[R](
    name: String,
    typedName: TypedName
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*
  import Liftables.TypedNameToExpr

  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[SelfRefRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typedName).asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "SelfRefRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
