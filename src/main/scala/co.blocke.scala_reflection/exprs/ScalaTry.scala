package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object ScalaTry:

  def makeExpr[T](scalaTry: TryRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val tt = scalaTry._tryType.toType(quotes)
    val optTypeExpr = stripType(ExprMaster.makeExpr(scalaTry._tryType)(using q)(using tt.asInstanceOf[Type[scalaTry._tryType.T]]).asInstanceOf[Expr[RType[scalaTry._tryType.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[ScalaOptionRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(scalaTry.name).asTerm,
            optTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]