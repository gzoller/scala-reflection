package co.blocke.scala_reflection
package rtypes

import Clazzes.*
import scala.math.{BigDecimal, BigInt}

trait PrimitiveRType:
  self: RType[?] =>
  val isNullable: Boolean

//------ SCALA ------
case class BigDecimalRType() extends RType[BigDecimal] with PrimitiveRType:
  val name = BIG_DECIMAL_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class BigIntRType() extends RType[BigInt] with PrimitiveRType:
  val name = BIG_INT_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class BooleanRType() extends RType[Boolean] with PrimitiveRType:
  val name = BOOLEAN_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class ByteRType() extends RType[Byte] with PrimitiveRType:
  val name = BYTE_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class CharRType() extends RType[Char] with PrimitiveRType:
  val name = CHAR_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class DoubleRType() extends RType[Double] with PrimitiveRType:
  val name = DOUBLE_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class FloatRType() extends RType[Float] with PrimitiveRType:
  val name = FLOAT_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class IntRType() extends RType[Int] with PrimitiveRType:
  val name = INT_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class LongRType() extends RType[Long] with PrimitiveRType:
  val name = LONG_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class ShortRType() extends RType[Short] with PrimitiveRType:
  val name = SHORT_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = false

case class StringRType() extends RType[String] with PrimitiveRType:
  val name = STRING_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class AnyRType() extends RType[Any] with PrimitiveRType:
  val name = ANY_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class AnyValRType() extends RType[AnyVal] with PrimitiveRType:
  val name = ANYVAL_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

//------ JAVA ------
case class JavaBigDecimalRType() extends RType[java.math.BigDecimal] with PrimitiveRType:
  val name = JBIG_DECIMAL_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaBigIntegerRType() extends RType[java.math.BigInteger] with PrimitiveRType:
  val name = JBIG_INTEGER_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaBooleanRType() extends RType[java.lang.Boolean] with PrimitiveRType:
  val name = JBOOLEAN_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaByteRType() extends RType[java.lang.Byte] with PrimitiveRType:
  val name = JBYTE_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaCharacterRType() extends RType[java.lang.Character] with PrimitiveRType:
  val name = JCHARACTER_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaDoubleRType() extends RType[java.lang.Double] with PrimitiveRType:
  val name = JDOUBLE_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaFloatRType() extends RType[java.lang.Float] with PrimitiveRType:
  val name = JFLOAT_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaIntegerRType() extends RType[java.lang.Integer] with PrimitiveRType:
  val name = JINTEGER_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaLongRType() extends RType[java.lang.Long] with PrimitiveRType:
  val name = JLONG_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaShortRType() extends RType[java.lang.Short] with PrimitiveRType:
  val name = JSHORT_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true

case class JavaNumberRType() extends RType[java.lang.Number] with PrimitiveRType:
  val name = JNUMBER_CLASS
  val typedName: TypedName = name
  override val isNullable: Boolean = true
