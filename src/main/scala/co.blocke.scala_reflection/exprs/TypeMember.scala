package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object TypeMember:

  def makeExpr(tm: TypeMemberRType)(using q:Quotes): Expr[RType[Any]] = 
  // def makeExpr[T](tm: TypeMemberRType)(using q:Quotes)(using Type[T]): Expr[RType[Any]] = 
    import q.reflect.*
    Apply(
        Select.unique(New(TypeTree.of[TypeMemberRType]),"<init>"), 
        List(
          Expr(tm.name).asTerm,
          Expr(tm.typeSymbol.asInstanceOf[String]).asTerm,
          ExprMaster.makeExpr(tm.memberType)(using q)(using RType.quotedTypeCache(tm.memberType.typedName).asInstanceOf[quoted.Type[tm.memberType.T]]).asTerm
        )
    ).asExprOf[RType[Any]]
