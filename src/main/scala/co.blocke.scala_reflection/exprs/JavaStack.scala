package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object JavaStack:

  def makeExpr[T](jstack: JavaStackRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val tt = jstack._elementType.toType(quotes)
    val jstackTypeExpr = stripType(ExprMaster.makeExpr(jstack._elementType)(using q)(using tt.asInstanceOf[Type[jstack._elementType.T]]).asInstanceOf[Expr[RType[jstack._elementType.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[JavaStackRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(jstack.name).asTerm,
            Expr(jstack.typeParamSymbols).asTerm,
            jstackTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
