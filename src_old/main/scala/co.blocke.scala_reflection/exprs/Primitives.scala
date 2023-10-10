package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Primitives:

  def makeExpr[T](prt: PrimitiveRType)(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    import q.reflect.*
    prt match {
      case t: BooleanRType       => '{ BooleanRType().asInstanceOf[RType[T]] }
      case t: ByteRType          => '{ ByteRType().asInstanceOf[RType[T]] }
      case t: CharRType          => '{ CharRType().asInstanceOf[RType[T]] }
      case t: DoubleRType        => '{ DoubleRType().asInstanceOf[RType[T]] }
      case t: FloatRType         => '{ FloatRType().asInstanceOf[RType[T]] }
      case t: IntRType           => '{ IntRType().asInstanceOf[RType[T]] }
      case t: LongRType          => '{ LongRType().asInstanceOf[RType[T]] }
      case t: ShortRType         => '{ ShortRType().asInstanceOf[RType[T]] }
      case t: StringRType        => '{ StringRType().asInstanceOf[RType[T]] }
      case t: AnyRType           => '{ AnyRType().asInstanceOf[RType[T]] }
      case t: JavaBooleanRType   => '{ JavaBooleanRType().asInstanceOf[RType[T]] }
      case t: JavaByteRType      => '{ JavaByteRType().asInstanceOf[RType[T]] }
      case t: JavaCharacterRType => '{ JavaCharacterRType().asInstanceOf[RType[T]] }
      case t: JavaDoubleRType    => '{ JavaDoubleRType().asInstanceOf[RType[T]] }
      case t: JavaFloatRType     => '{ JavaFloatRType().asInstanceOf[RType[T]] }
      case t: JavaIntegerRType   => '{ JavaIntegerRType().asInstanceOf[RType[T]] }
      case t: JavaLongRType      => '{ JavaLongRType().asInstanceOf[RType[T]] }
      case t: JavaShortRType     => '{ JavaShortRType().asInstanceOf[RType[T]] }
      case t: JavaObjectRType    => '{ JavaObjectRType().asInstanceOf[RType[T]] }
      case t: JavaNumberRType    => '{ JavaNumberRType().asInstanceOf[RType[T]] }
    }
