package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object LeftRight:

  def makeExpr[T](lr: LeftRightRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val ttLeft = lr._leftType.toType(quotes)
    val ttRight = lr._rightType.toType(quotes)
    val leftTypeExpr = stripType(ExprMaster.makeExpr(lr._leftType)(using q)(using ttLeft.asInstanceOf[Type[lr._leftType.T]]).asInstanceOf[Expr[RType[lr._leftType.T]]])
    val rightTypeExpr = stripType(ExprMaster.makeExpr(lr._rightType)(using q)(using ttRight.asInstanceOf[Type[lr._rightType.T]]).asInstanceOf[Expr[RType[lr._rightType.T]]])

    lr match {
        case scalaEither: EitherRType[_] =>
            Apply(
                TypeApply(
                    Select.unique(New(TypeTree.of[EitherRType[T]]),"<init>"), 
                    List(TypeTree.of[T])
                ),
                List(
                    Expr(scalaEither.name).asTerm,
                    leftTypeExpr.asTerm,
                    rightTypeExpr.asTerm
                )
            ).asExprOf[RType[T]]
    }