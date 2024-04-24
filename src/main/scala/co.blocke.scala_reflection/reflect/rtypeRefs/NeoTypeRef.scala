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

  // Ugly casting directly into internal Quotes/Compiler implementation. Eeek!
  // Only way to get to Symbol.info equivalent, which is only exposed "naturally" in experimental context,
  // which doesn't work for us at all.  This does the same thing, brute-forcing the issue.
  val wrappedTypeRef =
    val s = Symbol.requiredModule(typedName.toString).methodMember("validate").head.asInstanceOf[dotty.tools.dotc.core.Symbols.Symbol]
    implicit val c: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    s.denot.info.asInstanceOf[MethodType] match
      case MethodType(_, pps, _) =>
        reflect.ReflectOnType[T](quotes)(pps.head)(using scala.collection.mutable.Map.empty[TypedName, Boolean])

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
