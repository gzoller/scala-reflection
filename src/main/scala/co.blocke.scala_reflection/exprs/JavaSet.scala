package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object JavaSet:

  def makeExpr[T](jset: JavaSetRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val tt = jset._elementType.toType(quotes)
    val jsetTypeExpr = stripType(ExprMaster.makeExpr(jset._elementType)(using q)(using tt.asInstanceOf[Type[jset._elementType.T]]).asInstanceOf[Expr[RType[jset._elementType.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[JavaSetRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(jset.name).asTerm,
            Expr(jset.typeParamSymbols).asTerm,
            jsetTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
