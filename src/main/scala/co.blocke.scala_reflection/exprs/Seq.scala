package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Seq:

  def makeExpr[T](seq: SeqRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    val tt = seq._elementType.toType(quotes)
    val optTypeExpr = stripType(ExprMaster.makeExpr(seq._elementType)(using q)(using tt.asInstanceOf[Type[seq._elementType.T]]).asInstanceOf[Expr[RType[seq._elementType.T]]])

    Apply(
        TypeApply(
            Select.unique(New(TypeTree.of[SeqRType[T]]),"<init>"), 
            List(TypeTree.of[T])
        ),
        List(
            Expr(seq.name).asTerm,
            optTypeExpr.asTerm
        )
    ).asExprOf[RType[T]]
