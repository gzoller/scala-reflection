package co.blocke.scala_reflection
package rtypes

import scala.quoted.*
import Clazzes.*

// === Scala Types
// ===============

case class BooleanRType() extends RType[Boolean] with PrimitiveRType {
  val name = BOOLEAN_CLASS
  val typedName = BOOLEAN_CLASS
  lazy val clazz = BooleanClazz
}

case class ByteRType() extends RType[Byte] with PrimitiveRType {
  val name = BYTE_CLASS
  val typedName = BYTE_CLASS
  lazy val clazz = ByteClazz
}

case class CharRType() extends RType[Char] with PrimitiveRType {
  val name = CHAR_CLASS
  val typedName = CHAR_CLASS
  lazy val clazz = CharClazz
}

case class DoubleRType() extends RType[Double] with PrimitiveRType {
  val name = DOUBLE_CLASS
  val typedName = DOUBLE_CLASS
  lazy val clazz = DoubleClazz
}

case class FloatRType() extends RType[Float] with PrimitiveRType {
  val name = FLOAT_CLASS
  val typedName = FLOAT_CLASS
  lazy val clazz = FloatClazz
}

case class IntRType() extends RType[Int] with PrimitiveRType {
  val name = INT_CLASS
  val typedName = INT_CLASS
  lazy val clazz = IntClazz
}

case class LongRType() extends RType[Long] with PrimitiveRType {
  val name = LONG_CLASS
  val typedName = LONG_CLASS
  lazy val clazz = LongClazz
}

case class ShortRType() extends RType[Short] with PrimitiveRType {
  val name = SHORT_CLASS
  val typedName = SHORT_CLASS
  lazy val clazz = ShortClazz
}

case class StringRType() extends RType[String] with PrimitiveRType {
  val name = STRING_CLASS
  val typedName = STRING_CLASS
  lazy val clazz = StringClazz
}

case class AnyRType() extends RType[Any] with PrimitiveRType {
  val name = ANY_CLASS
  val typedName = ANY_CLASS
  lazy val clazz = AnyClazz
}

// === Java Types
// ==============

case class JavaBooleanRType() extends RType[java.lang.Boolean] with PrimitiveRType {
  val name = JBooleanClazz.getName
  val typedName = JBooleanClazz.getName
  lazy val clazz = JBooleanClazz
}

case class JavaByteRType() extends RType[java.lang.Byte] with PrimitiveRType {
  val name = JByteClazz.getName
  val typedName = JByteClazz.getName
  lazy val clazz = JByteClazz
}

case class JavaCharacterRType() extends RType[java.lang.Character] with PrimitiveRType {
  val name = JCharacterClazz.getName
  val typedName = JCharacterClazz.getName
  lazy val clazz = JCharacterClazz
}

case class JavaDoubleRType() extends RType[java.lang.Double] with PrimitiveRType {
  val name = JDoubleClazz.getName
  val typedName = JDoubleClazz.getName
  lazy val clazz = JDoubleClazz
}

case class JavaFloatRType() extends RType[java.lang.Float] with PrimitiveRType {
  val name = JFloatClazz.getName
  val typedName = JFloatClazz.getName
  lazy val clazz = JFloatClazz
}

case class JavaIntegerRType() extends RType[java.lang.Integer] with PrimitiveRType {
  val name = JIntegerClazz.getName
  val typedName = JIntegerClazz.getName
  lazy val clazz = JIntegerClazz
}

case class JavaLongRType() extends RType[java.lang.Long] with PrimitiveRType {
  val name = JLongClazz.getName
  val typedName = JLongClazz.getName
  lazy val clazz = JLongClazz
}

case class JavaShortRType() extends RType[java.lang.Short] with PrimitiveRType {
  val name = JShortClazz.getName
  val typedName = JShortClazz.getName
  lazy val clazz = JShortClazz
}

case class JavaObjectRType() extends RType[java.lang.Object] with PrimitiveRType {
  val name = ObjectClazz.getName
  val typedName = ObjectClazz.getName
  lazy val clazz = ObjectClazz
}

case class JavaNumberRType() extends RType[java.lang.Number] with PrimitiveRType {
  val name = JNumberClazz.getName
  val typedName = JNumberClazz.getName
  lazy val clazz = JNumberClazz
}

object PrimitiveRTypes:
  def loadCache(): Map[TypedName, RType[_]] =

    val boolRType = BooleanRType()
    val byteRType = ByteRType()
    val charRType = CharRType()
    val doubleRType = DoubleRType()
    val floatRType = FloatRType()
    val intRType = IntRType()
    val longRType = LongRType()
    val shortRType = ShortRType()
    val stringRType = StringRType()
    val anyRType = AnyRType()

    given string2typednamekey: Conversion[(String, RType[_]), (TypedName, RType[_])] with
      def apply(x: (String, RType[?])): (TypedName, RType[?]) = (x._1.asInstanceOf[TypedName], x._2)

    // Do NOT pre-populate Any type into the cache!  Doing so will erroneously convert any opaque type
    // usages to type symbols.
    Map(
      "boolean" -> boolRType,
      "Boolean" -> boolRType,
      "scala.Boolean" -> boolRType,
      "java.lang.Boolean" -> JavaBooleanRType(),
      "byte" -> byteRType,
      "Byte" -> byteRType,
      "scala.Byte" -> byteRType,
      "java.lang.Byte" -> JavaByteRType(),
      "char" -> charRType,
      "Char" -> charRType,
      "scala.Char" -> charRType,
      "java.lang.Character" -> JavaCharacterRType(),
      "double" -> doubleRType,
      "Double" -> doubleRType,
      "scala.Double" -> doubleRType,
      "java.lang.Double" -> JavaDoubleRType(),
      "float" -> floatRType,
      "Float" -> floatRType,
      "scala.Float" -> floatRType,
      "java.lang.Float" -> JavaFloatRType(),
      "int" -> intRType,
      "Int" -> intRType,
      "scala.Int" -> intRType,
      "java.lang.Integer" -> JavaIntegerRType(),
      "long" -> longRType,
      "Long" -> longRType,
      "scala.Long" -> longRType,
      "java.lang.Long" -> JavaLongRType(),
      "short" -> shortRType,
      "Short" -> shortRType,
      "scala.Short" -> shortRType,
      "java.lang.Short" -> JavaShortRType(),
      "java.lang.String" -> stringRType,
      "java.lang.Object" -> JavaObjectRType(),
      "java.lang.Number" -> JavaNumberRType()
    )
