package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Map:

  def makeExpr[T](map: MapRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    val ttKey = map.elementType.toType(quotes)
    val ttValue = map.elementType2.toType(quotes)
    val keyTypeExpr = stripType(
      ExprMaster
        .makeExpr(map.elementType)(using q)(using ttKey.asInstanceOf[Type[map.elementType.T]])
        .asInstanceOf[Expr[RType[map.elementType.T]]]
    )
    val valueTypeExpr = stripType(
      ExprMaster
        .makeExpr(map.elementType2)(using q)(using ttValue.asInstanceOf[Type[map.elementType2.T]])
        .asInstanceOf[Expr[RType[map.elementType2.T]]]
    )

    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[MapRType[T]]), "<init>"),
        List(TypeTree.of[T])
      ),
      List(
        Expr(map.name).asTerm,
        Expr(map.typeParamSymbols).asTerm,
        keyTypeExpr.asTerm,
        valueTypeExpr.asTerm
      )
    ).asExprOf[RType[T]]
