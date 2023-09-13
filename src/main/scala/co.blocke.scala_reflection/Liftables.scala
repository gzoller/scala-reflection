package co.blocke.scala_reflection

import scala.quoted.*

object Liftables:

  // Allow Expr(RType[T])
  // given RTypeToExpr[T: Type: ToExpr]: ToExpr[RType[T]] with
  //   def apply(rt: RType[T])(using Quotes): Expr[RType[T]] = rt match 
  //     case r: RType[T] => Expr(r)

  given TypeSymbolToExpr: ToExpr[TypeSymbol] with {
    def apply(x: TypeSymbol)(using Quotes): Expr[TypeSymbol] = Expr(x)
  }

  given OptTypeSymbolToExpr: ToExpr[Option[TypeSymbol]] with {
    def apply(x: Option[TypeSymbol])(using Quotes): Expr[Option[TypeSymbol]] = Expr(x)
  }

  given TypedNameToExpr: ToExpr[TypedName] with {
    def apply(x: TypedName)(using Quotes): Expr[TypedName] = Expr(x.toString.asInstanceOf[TypedName])
  }
