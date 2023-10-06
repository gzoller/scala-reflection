package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Enums:

  def makeExpr[T](e: EnumRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    e match {
      case s: ScalaEnumerationRType[?] => // Scala 2.x Enumeration
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[ScalaEnumerationRType[T]]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(s.name).asTerm,
            Expr(s.values).asTerm
          )
        ).asExprOf[RType[T]]

      case s: ScalaEnumRType[?] => // Scala 3.x Enum
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[ScalaEnumRType[T]]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(s.name).asTerm,
            Expr(s.values).asTerm
          )
        ).asExprOf[RType[T]]
    }
