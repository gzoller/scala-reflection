package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object JavaList:

  def makeExpr[T](jlist: JavaListRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val tt = jlist._elementType.toType(quotes)
    val jlistTypeExpr = stripType(ExprMaster.makeExpr(jlist._elementType)(using q)(using tt.asInstanceOf[Type[jlist._elementType.T]]).asInstanceOf[Expr[RType[jlist._elementType.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[JavaListRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(jlist.name).asTerm,
            Expr(jlist.typeParamSymbols).asTerm,
            jlistTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
