package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object LeftRight:

  def makeExpr[T](lr: LeftRightRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    val ttLeft = lr.leftType.toType(quotes)
    val ttRight = lr.rightType.toType(quotes)
    val leftTypeExpr = stripType(
      ExprMaster
        .makeExpr(lr.leftType)(using q)(using ttLeft.asInstanceOf[Type[lr.leftType.T]])
        .asInstanceOf[Expr[RType[lr.leftType.T]]]
    )
    val rightTypeExpr = stripType(
      ExprMaster
        .makeExpr(lr.rightType)(using q)(using ttRight.asInstanceOf[Type[lr.rightType.T]])
        .asInstanceOf[Expr[RType[lr.rightType.T]]]
    )

    lr match {
      case scalaEither: EitherRType[?] =>
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[EitherRType[T]]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(scalaEither.name).asTerm,
            Expr(scalaEither.typeParamSymbols).asTerm,
            leftTypeExpr.asTerm,
            rightTypeExpr.asTerm
          )
        ).asExprOf[RType[T]]
      case union: UnionRType[?] =>
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[UnionRType[T]]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(union.name).asTerm,
            Expr(union.typeParamSymbols).asTerm,
            leftTypeExpr.asTerm,
            rightTypeExpr.asTerm
          )
        ).asExprOf[RType[T]]
      case intersection: IntersectionRType[?] =>
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[IntersectionRType[T]]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(intersection.name).asTerm,
            Expr(intersection.typeParamSymbols).asTerm,
            leftTypeExpr.asTerm,
            rightTypeExpr.asTerm
          )
        ).asExprOf[RType[T]]
    }
