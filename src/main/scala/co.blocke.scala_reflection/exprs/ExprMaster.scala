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
    println("MASTER: "+rt)
    rt match {
      case primitive: PrimitiveRType => Primitives.makeExpr(primitive)
      case clazz: ClassRType[T] => Classes.makeExpr(clazz)
      case typeMember: TypeMemberRType[T] => TypeMember.makeExpr(typeMember)
      case unknown: UnknownRType => '{ UnknownRType( ${Expr(unknown.name)} ).asInstanceOf[RType[T]] }
    }


  def makeFieldExpr[T]( fieldInfo: FieldInfo[T] )(using q:Quotes)(using Type[T]): Expr[FieldInfo[T]] =
    import q.reflect.*
    import Liftables.OptTypeSymbolToExpr

    println("Making field: "+fieldInfo.name)
    val fi = fieldInfo.asInstanceOf[ScalaFieldInfo[T]]
    val typedFieldInfo = fi.fieldType.asInstanceOf[RType[fi.fieldType.T]]
    val tt = fi.fieldType.toType(quotes).asInstanceOf[Type[fi.fieldType.T]]
    val fieldTypeExpr = ExprMaster.makeExpr(typedFieldInfo)(using quotes)(using tt).asInstanceOf[Expr[RType[fi.fieldType.T]]]
    println("Ready field: "+fieldInfo.name)
 
    '{ 
      ScalaFieldInfo[T](
        ${Expr(fi.index)}, 
        ${Expr(fi.name)},
        ${ fieldTypeExpr }, 
        ${Expr(fi.annotations)}, 
        // ${Expr(fi.defaultValueAccessorName)}, 
        // ${Expr(fi.originalSymbol)},
        // ${Expr(fi.isNonValConstructorField)}
     ).asInstanceOf[FieldInfo[T]]
    }
