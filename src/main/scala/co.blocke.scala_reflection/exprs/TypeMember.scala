package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object TypeMember:

  def makeExpr[T](tm: TypeMemberRType)(using q:Quotes)(using Type[T]): Expr[RType[Any]] = 
    import q.reflect.*
    val tt = TypeRepr.of[T].asType.asInstanceOf[Type[tm.memberType.T]]
    Apply(
        Select.unique(New(TypeTree.of[TypeMemberRType]),"<init>"), 
        List(
          Expr(tm.name).asTerm,
          Expr(tm.typeSymbol.asInstanceOf[String]).asTerm,
          ExprMaster.makeExpr(tm.memberType)(using q)(using tt).asTerm
        )
    ).asExprOf[RType[Any]]
