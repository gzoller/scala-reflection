package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.NeoTypeRType
import util.{JsonField, JsonObjectBuilder}

/** Ideally we'd like to know the actual wrapped type in the NeoTypeRef, but sadly this is
  * impossible to know at compile time.  Reflection here only gives us a Type and nothing
  * more. The 'given' at runtime actually binds it to an actual (typed) implementation.
  * So here we just record that this is a NeoType of a given type name and reftype.
  *
  * @param name
  * @param quotes
  * @param tt
  */
case class NeoTypeRef[R](
    name: String,
    typedName: TypedName
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*
  import Liftables.TypedNameToExpr

  val refType = tt

  val unitVal = '{ null.asInstanceOf[R] }

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[NeoTypeRType[R]]), "<init>"),
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
        JsonField("rtype", "NeoTypeRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
