package co.blocke.scala_reflection

import scala.quoted.*

object Liftables:

  given TypeSymbolToExpr: ToExpr[TypeSymbol] with {
    def apply(x: TypeSymbol)(using Quotes): Expr[TypeSymbol] =
      val s = Expr(x.toString)
      '{ $s.asInstanceOf[TypeSymbol] }
  }

  given OptTypeSymbolToExpr: ToExpr[Option[TypeSymbol]] with {
    def apply(x: Option[TypeSymbol])(using Quotes): Expr[Option[TypeSymbol]] =
      val opt = Expr(x.map(_.toString))
      '{ $opt.asInstanceOf[Option[TypeSymbol]] }
  }

  given ListTypeSymbolToExpr: ToExpr[List[TypeSymbol]] with {
    def apply(x: List[TypeSymbol])(using Quotes): Expr[List[TypeSymbol]] =
      val syms = Expr(x.map(_.toString))
      '{ $syms.asInstanceOf[List[TypeSymbol]] }
  }

  given TypedNameToExpr: ToExpr[TypedName] with {
    def apply(x: TypedName)(using Quotes): Expr[TypedName] =
      val s = Expr(x.toString)
      '{ $s.asInstanceOf[TypedName] }
  }
