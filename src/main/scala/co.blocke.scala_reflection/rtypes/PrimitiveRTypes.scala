package co.blocke.scala_reflection
package rtypes

import Clazzes.*

trait PrimitiveRType:
  self: RType[?] =>

//------ SCALA ------
case class BooleanRType() extends RType[Boolean] with PrimitiveRType:
  val name = BOOLEAN_CLASS
  val typedName: TypedName = name

case class ByteRType() extends RType[Byte] with PrimitiveRType:
  val name = BYTE_CLASS
  val typedName: TypedName = name

case class CharRType() extends RType[Char] with PrimitiveRType:
  val name = CHAR_CLASS
  val typedName: TypedName = name

case class DoubleRType() extends RType[Double] with PrimitiveRType:
  val name = DOUBLE_CLASS
  val typedName: TypedName = name

case class FloatRType() extends RType[Float] with PrimitiveRType:
  val name = FLOAT_CLASS
  val typedName: TypedName = name

case class IntRType() extends RType[Int] with PrimitiveRType:
  val name = INT_CLASS
  val typedName: TypedName = name

case class LongRType() extends RType[Long] with PrimitiveRType:
  val name = LONG_CLASS
  val typedName: TypedName = name

case class ShortRType() extends RType[Short] with PrimitiveRType:
  val name = SHORT_CLASS
  val typedName: TypedName = name

case class StringRType() extends RType[String] with PrimitiveRType:
  val name = STRING_CLASS
  val typedName: TypedName = name

case class AnyRType() extends RType[Any] with PrimitiveRType:
  val name = ANY_CLASS
  val typedName: TypedName = name

//------ JAVA ------
case class JavaBooleanRType() extends RType[java.lang.Boolean] with PrimitiveRType:
  val name = JBOOLEAN_CLASS
  val typedName: TypedName = name

case class JavaByteRType() extends RType[java.lang.Byte] with PrimitiveRType:
  val name = JBYTE_CLASS
  val typedName: TypedName = name

case class JavaCharacterRType() extends RType[java.lang.Character] with PrimitiveRType:
  val name = JCHARACTER_CLASS
  val typedName: TypedName = name

case class JavaDoubleRType() extends RType[java.lang.Double] with PrimitiveRType:
  val name = JDOUBLE_CLASS
  val typedName: TypedName = name

case class JavaFloatRType() extends RType[java.lang.Float] with PrimitiveRType:
  val name = JFLOAT_CLASS
  val typedName: TypedName = name

case class JavaIntegerRType() extends RType[java.lang.Integer] with PrimitiveRType:
  val name = JINTEGER_CLASS
  val typedName: TypedName = name

case class JavaLongRType() extends RType[java.lang.Long] with PrimitiveRType:
  val name = JLONG_CLASS
  val typedName: TypedName = name

case class JavaShortRType() extends RType[java.lang.Short] with PrimitiveRType:
  val name = JSHORT_CLASS
  val typedName: TypedName = name

case class JavaObjectRType() extends RType[java.lang.Object] with PrimitiveRType:
  val name = JOBJECT_CLASS
  val typedName: TypedName = name

case class JavaNumberRType() extends RType[java.lang.Number] with PrimitiveRType:
  val name = JNUMBER_CLASS
  val typedName: TypedName = name
