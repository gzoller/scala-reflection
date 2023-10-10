package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object SelfRef:

  def makeExpr[T](sr: SelfRefRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypedNameToExpr

    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[SelfRefRType[T]]), "<init>"),
        List(TypeTree.of[T])
      ),
      List(
        Expr(sr.name).asTerm,
        Expr(sr.typedName).asTerm
      )
    ).asExprOf[RType[T]]
