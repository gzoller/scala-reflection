package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Seq:

  def makeExpr[T](seq: SeqRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    val tt = seq.elementType.toType(quotes)
    val seqTypeExpr = stripType(
      ExprMaster
        .makeExpr(seq.elementType)(using q)(using tt.asInstanceOf[Type[seq.elementType.T]])
        .asInstanceOf[Expr[RType[seq.elementType.T]]]
    )

    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[SeqRType[T]]), "<init>"),
        List(TypeTree.of[T])
      ),
      List(
        Expr(seq.name).asTerm,
        Expr(seq.typeParamSymbols).asTerm,
        seqTypeExpr.asTerm
      )
    ).asExprOf[RType[T]]
