package co.blocke.scala_reflection
package rtypes

import Clazzes.*

trait PrimitiveRType:
  self: RType[?] =>
  val typedName: TypedName = self.name

//------ SCALA ------
case class BooleanRType() extends RType[Boolean] with PrimitiveRType:
  val name = BOOLEAN_CLASS

case class ByteRType() extends RType[Byte] with PrimitiveRType:
  val name = BYTE_CLASS

case class CharRType() extends RType[Char] with PrimitiveRType:
  val name = CHAR_CLASS

case class DoubleRType() extends RType[Double] with PrimitiveRType:
  val name = DOUBLE_CLASS

case class FloatRType() extends RType[Float] with PrimitiveRType:
  val name = FLOAT_CLASS

case class IntRType() extends RType[Int] with PrimitiveRType:
  val name = INT_CLASS

case class LongRType() extends RType[Long] with PrimitiveRType:
  val name = LONG_CLASS

case class ShortRType() extends RType[Short] with PrimitiveRType:
  val name = SHORT_CLASS

case class StringRType() extends RType[String] with PrimitiveRType:
  val name = STRING_CLASS

case class AnyRType() extends RType[Any] with PrimitiveRType:
  val name = ANY_CLASS

//------ JAVA ------
case class JavaBooleanRType() extends RType[java.lang.Boolean] with PrimitiveRType:
  val name = JBOOLEAN_CLASS

case class JavaByteRType() extends RType[java.lang.Byte] with PrimitiveRType:
  val name = JBYTE_CLASS

case class JavaCharacterRType() extends RType[java.lang.Character] with PrimitiveRType:
  val name = JCHARACTER_CLASS

case class JavaDoubleRType() extends RType[java.lang.Double] with PrimitiveRType:
  val name = JDOUBLE_CLASS

case class JavaFloatRType() extends RType[java.lang.Float] with PrimitiveRType:
  val name = JFLOAT_CLASS

case class JavaIntegerRType() extends RType[java.lang.Integer] with PrimitiveRType:
  val name = JINTEGER_CLASS

case class JavaLongRType() extends RType[java.lang.Long] with PrimitiveRType:
  val name = JLONG_CLASS

case class JavaShortRType() extends RType[java.lang.Short] with PrimitiveRType:
  val name = JSHORT_CLASS

case class JavaObjectRType() extends RType[java.lang.Object] with PrimitiveRType:
  val name = JOBJECT_CLASS

case class JavaNumberRType() extends RType[java.lang.Number] with PrimitiveRType:
  val name = JNUMBER_CLASS
