package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object JavaQueue:

  def makeExpr[T](jq: JavaQueueRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val tt = jq._elementType.toType(quotes)
    val jqTypeExpr = stripType(ExprMaster.makeExpr(jq._elementType)(using q)(using tt.asInstanceOf[Type[jq._elementType.T]]).asInstanceOf[Expr[RType[jq._elementType.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[JavaQueueRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(jq.name).asTerm,
            Expr(jq.typeParamSymbols).asTerm,
            jqTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
