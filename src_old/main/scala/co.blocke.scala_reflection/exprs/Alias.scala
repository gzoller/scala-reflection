package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Alias:

  def makeExpr[T](a: AliasRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    val tt = a.unwrappedType.toType(quotes)
    val typeExpr = stripType(
      ExprMaster
        .makeExpr(a.unwrappedType)(using q)(using tt.asInstanceOf[Type[a.unwrappedType.T]])
        .asInstanceOf[Expr[RType[a.unwrappedType.T]]]
    )

    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[AliasRType[T]]), "<init>"),
        List(TypeTree.of[T])
      ),
      List(
        Expr(a.definedType).asTerm,
        typeExpr.asTerm
      )
    ).asExprOf[RType[T]]
