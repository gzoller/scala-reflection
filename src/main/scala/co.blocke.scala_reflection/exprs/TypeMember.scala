package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object TypeMember:

  def makeExpr[T](tm: TypeMemberRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    Apply(
        Select.unique(New(TypeTree.of[TypeMemberRType]),"<init>"), 
        List(
          Expr(tm.name).asTerm,
          Expr(tm.typeSymbol.asInstanceOf[String]).asTerm,
          ExprMaster.makeExpr(tm.memberType).asTerm
        )
    ).asExprOf[RType[T]]
