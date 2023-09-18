package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Array:

  def makeExpr[T](arr: ArrayRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val tt = arr._elementType.toType(quotes)
    val arrTypeExpr = stripType(ExprMaster.makeExpr(arr._elementType)(using q)(using tt.asInstanceOf[Type[arr._elementType.T]]).asInstanceOf[Expr[RType[arr._elementType.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[ArrayRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(arr.name).asTerm,
            arrTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
