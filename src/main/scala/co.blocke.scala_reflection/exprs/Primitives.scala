package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Primitives:

  def makeExpr[T](prt: PrimitiveRType)(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    prt match {
      case t: BooleanRType => '{ BooleanRType().asInstanceOf[RType[T]] }
      case t: ByteRType => '{ ByteRType().asInstanceOf[RType[T]] }
      case t: CharRType => '{ CharRType().asInstanceOf[RType[T]] }
      case t: DoubleRType => '{ DoubleRType().asInstanceOf[RType[T]] }
      case t: FloatRType => '{ FloatRType().asInstanceOf[RType[T]] }
      case t: IntRType => '{ IntRType().asInstanceOf[RType[T]] }
      case t: LongRType => '{ LongRType().asInstanceOf[RType[T]] }
      case t: ShortRType => '{ ShortRType().asInstanceOf[RType[T]] }
      case t: StringRType => '{ StringRType().asInstanceOf[RType[T]] }
      case t: AnyRType => '{ AnyRType().asInstanceOf[RType[T]] }
    }
