package co.blocke.scala_reflection
package rtypes

import scala.quoted.*

case class BooleanRType() extends RType[Boolean] with PrimitiveRType {  val name = BOOLEAN_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class ByteRType() extends RType[Byte] with PrimitiveRType{ val name = BYTE_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class CharRType() extends RType[Char] with PrimitiveRType{ val name = CHAR_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class DoubleRType() extends RType[Double] with PrimitiveRType{ val name = DOUBLE_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class FloatRType() extends RType[Float] with PrimitiveRType{ val name = FLOAT_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class IntRType() extends RType[Int] with PrimitiveRType{ val name = INT_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class LongRType() extends RType[Long] with PrimitiveRType{ val name = LONG_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class ShortRType() extends RType[Short] with PrimitiveRType{ val name = SHORT_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class StringRType() extends RType[String] with PrimitiveRType{ val name = STRING_CLASS; val typedName = name.asInstanceOf[TypedName] }
case class AnyRType() extends RType[Any] with PrimitiveRType{ val name = ANY_CLASS; val typedName = name.asInstanceOf[TypedName] }


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

    given string2typednamekey: Conversion[(String,RType[_]), (TypedName,RType[_])] with
      def apply(x: (String,RType[_])): (TypedName,RType[_]) = (x._1.asInstanceOf[TypedName], x._2)

    Map(
      "boolean"              -> boolRType,
      "Boolean"              -> boolRType,
      "scala.Boolean"        -> boolRType,
    //   "java.lang.Boolean"    -> Java_Boolean,
      "byte"                 -> byteRType,
      "Byte"                 -> byteRType,
      "scala.Byte"           -> byteRType,
    //   "java.lang.Byte"       -> Java_Byte,
      "char"                 -> charRType,
      "Char"                 -> charRType,
      "scala.Char"           -> charRType,
    //   "java.lang.Character"  -> Java_Char,
      "double"               -> doubleRType,
      "Double"               -> doubleRType,
      "scala.Double"         -> doubleRType,
    //   "java.lang.Double"     -> Java_Double,
      "float"                -> floatRType,
      "Float"                -> floatRType,
      "scala.Float"          -> floatRType,
    //   "java.lang.Float"      -> Java_Float,
      "int"                  -> intRType,
      "Int"                  -> intRType,
      "scala.Int"            -> intRType,
    //   "java.lang.Integer"    -> Java_Int,
      "long"                 -> longRType,
      "Long"                 -> longRType,
      "scala.Long"           -> longRType,
    //   "java.lang.Long"       -> Java_Long,
      "short"                -> shortRType,
      "Short"                -> shortRType,
      "scala.Short"          -> shortRType,
    //   "java.lang.Short"      -> Java_Short,
      "java.lang.String"     -> stringRType,
      "scala.Any"            -> anyRType
    //   "java.lang.Object"     -> Java_Object,
    //   "java.lang.Number"     -> Java_Number
    )