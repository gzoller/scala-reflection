package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Options:

  def makeExpr[T](opt: OptionRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

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
                    Expr(scalaOpt.typeParamSymbols).asTerm,
                    optTypeExpr.asTerm
                )
            ).asExprOf[RType[T]]

        case javaOpt: JavaOptionalRType[_] =>
            val tt = javaOpt._optionParamType.toType(quotes)
            val optTypeExpr = stripType(ExprMaster.makeExpr(javaOpt._optionParamType)(using q)(using tt.asInstanceOf[Type[javaOpt._optionParamType.T]]).asInstanceOf[Expr[RType[javaOpt._optionParamType.T]]])

            Apply(
                TypeApply(
                    Select.unique(New(TypeTree.of[JavaOptionalRType[T]]),"<init>"), 
                    List(TypeTree.of[T])
                ),
                List(
                    Expr(javaOpt.name).asTerm,
                    Expr(javaOpt.typeParamSymbols).asTerm,
                    optTypeExpr.asTerm
                )
            ).asExprOf[RType[T]]
    }