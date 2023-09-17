package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Options:

  def makeExpr[T](opt: OptionRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    opt match {
        case scalaOpt: ScalaOptionRType[_] =>

            val tt = scalaOpt._optionParamType.toType(quotes)
            val optTypeExpr = stripType(ExprMaster.makeExpr(scalaOpt._optionParamType)(using q)(using tt.asInstanceOf[Type[scalaOpt._optionParamType.T]]).asInstanceOf[Expr[RType[scalaOpt._optionParamType.T]]])

            Apply(
                TypeApply(
                    Select.unique(New(TypeTree.of[ScalaOptionRType[T]]),"<init>"), 
                    List(TypeTree.of[T])
                ),
                List(
                    Expr(scalaOpt.name).asTerm,
                    optTypeExpr.asTerm
                )
            ).asExprOf[RType[T]]
    }