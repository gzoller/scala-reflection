package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object ScalaTry:

  def makeExpr[T](scalaTry: TryRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    val tt = scalaTry.tryType.toType(quotes)
    val tryTypeExpr = stripType(
      ExprMaster
        .makeExpr(scalaTry.tryType)(using q)(using tt.asInstanceOf[Type[scalaTry.tryType.T]])
        .asInstanceOf[Expr[RType[scalaTry.tryType.T]]]
    )

    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[ScalaOptionRType[T]]), "<init>"),
        List(TypeTree.of[T])
      ),
      List(
        Expr(scalaTry.name).asTerm,
        Expr(scalaTry.typeParamSymbols).asTerm,
        tryTypeExpr.asTerm
      )
    ).asExprOf[RType[T]]
