package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Map:

  def makeExpr[T](map: MapRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val ttKey = map._elementType.toType(quotes)
    val ttValue = map._elementType2.toType(quotes)
    val keyTypeExpr = stripType(ExprMaster.makeExpr(map._elementType)(using q)(using ttKey.asInstanceOf[Type[map._elementType.T]]).asInstanceOf[Expr[RType[map._elementType.T]]])
    val valueTypeExpr = stripType(ExprMaster.makeExpr(map._elementType2)(using q)(using ttValue.asInstanceOf[Type[map._elementType2.T]]).asInstanceOf[Expr[RType[map._elementType2.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[MapRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(map.name).asTerm,
            keyTypeExpr.asTerm,
            valueTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
