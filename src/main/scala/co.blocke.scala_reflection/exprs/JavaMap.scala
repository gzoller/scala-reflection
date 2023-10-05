package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object JavaMap:

  def makeExpr[T](jmap: JavaMapRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val ttKey = jmap._elementType.toType(quotes)
    val ttValue = jmap._elementType2.toType(quotes)
    val keyTypeExpr = stripType(ExprMaster.makeExpr(jmap._elementType)(using q)(using ttKey.asInstanceOf[Type[jmap._elementType.T]]).asInstanceOf[Expr[RType[jmap._elementType.T]]])
    val valueTypeExpr = stripType(ExprMaster.makeExpr(jmap._elementType2)(using q)(using ttValue.asInstanceOf[Type[jmap._elementType2.T]]).asInstanceOf[Expr[RType[jmap._elementType2.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[JavaMapRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(jmap.name).asTerm,
            Expr(jmap.typeParamSymbols).asTerm,
            keyTypeExpr.asTerm,
            valueTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
