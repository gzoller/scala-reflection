package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

/*
Master object that creates all RType[T] Expr's.  Feed it an RType[T] and await the Expr[T].
When building Expr[T]s for classes there will likely be lots of recursion, so this is the
safe common entry point for whenever you need to descend into an RType to make an Expr.
*/

object ExprMaster:

  def makeExpr[T](rt: RType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    rt match {
      case primitive: PrimitiveRType => Primitives.makeExpr(primitive) // primitive types already preloaded in exprCache
      case clazz: ClassRType[T] => Classes.makeExpr(clazz)
      case opt: OptionRType[T] => Options.makeExpr(opt)
      case typeMember: TypeMemberRType => TypeMember.makeExpr(typeMember)
      case typeSymbol: TypeSymbolRType => '{ TypeSymbolRType( ${Expr(typeSymbol.name)} ).asInstanceOf[RType[T]] }
      case selfRef: SelfRefRType[T] => SelfRef.makeExpr(selfRef)
      case unknown: UnknownRType => '{ UnknownRType( ${Expr(unknown.name)} ).asInstanceOf[RType[T]] }
    }

