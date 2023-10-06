package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Options:

  def makeExpr[T](opt: OptionRType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.TypeSymbolToExpr

    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    opt match {
      case scalaOpt: ScalaOptionRType[?] =>
        val tt = scalaOpt.optionParamType.toType(quotes)
        val optTypeExpr = stripType(
          ExprMaster
            .makeExpr(scalaOpt.optionParamType)(using q)(using tt.asInstanceOf[Type[scalaOpt.optionParamType.T]])
            .asInstanceOf[Expr[RType[scalaOpt.optionParamType.T]]]
        )

        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[ScalaOptionRType[T]]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(scalaOpt.name).asTerm,
            Expr(scalaOpt.typeParamSymbols).asTerm,
            optTypeExpr.asTerm
          )
        ).asExprOf[RType[T]]

      case javaOpt: JavaOptionalRType[?] =>
        val tt = javaOpt.optionParamType.toType(quotes)
        val optTypeExpr = stripType(
          ExprMaster
            .makeExpr(javaOpt.optionParamType)(using q)(using tt.asInstanceOf[Type[javaOpt.optionParamType.T]])
            .asInstanceOf[Expr[RType[javaOpt.optionParamType.T]]]
        )

        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[JavaOptionalRType[T]]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(javaOpt.name).asTerm,
            Expr(javaOpt.typeParamSymbols).asTerm,
            optTypeExpr.asTerm
          )
        ).asExprOf[RType[T]]
    }
