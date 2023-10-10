package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Tuple:

  def makeExpr[T](tuple: TupleRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    val tTypes = tuple.tupleTypes.map(_.toType(quotes))
    val tupleTypeExpr = tuple.tupleTypes.zip(tTypes).map { case (rt, rtType) =>
      stripType(
        ExprMaster.makeExpr(rt)(using q)(using rtType.asInstanceOf[Type[rt.T]]).asInstanceOf[Expr[RType[rt.T]]]
      )
    }

    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[TupleRType[T]]), "<init>"),
        List(TypeTree.of[T])
      ),
      List(
        Expr(tuple.name).asTerm,
        Expr(tuple.typeParamSymbols).asTerm,
        Expr.ofList(tupleTypeExpr).asTerm
      )
    ).asExprOf[RType[T]]
