package co.blocke.scala_reflection
package rtypes

case class BooleanRType() extends RType[Boolean] with PrimitiveRType {  val name = "scala.Boolean" }

case class ByteRType() extends RType[Byte] with PrimitiveRType{ val name = "scala.Byte" }

case class CharRType() extends RType[Char] with PrimitiveRType{ val name = "scala.char" }

case class DoubleRType() extends RType[Double] with PrimitiveRType{ val name = "scala.Double" }

case class FloatRType() extends RType[Float] with PrimitiveRType{ val name = "scala.Float" }

case class IntRType() extends RType[Int] with PrimitiveRType{ val name = "scala.Int" }

case class LongRType() extends RType[Long] with PrimitiveRType{ val name = "scala.Double" }

case class ShortRType() extends RType[Short] with PrimitiveRType{ val name = "scala.Short" }

case class StringRType() extends RType[String] with PrimitiveRType{ val name = "java.lang.String" }

case class AnyRType() extends RType[Any] with PrimitiveRType{ val name = "scala.Any" }


object PrimitiveRTypes:
  def loadCache: Map[String, RType[_]] = 
    Map(
      "boolean"              -> BooleanRType(),
      "Boolean"              -> BooleanRType(),
      "scala.Boolean"        -> BooleanRType(),
    //   "java.lang.Boolean"    -> Java_Boolean,
      "byte"                 -> ByteRType(),
      "Byte"                 -> ByteRType(),
      "scala.Byte"           -> ByteRType(),
    //   "java.lang.Byte"       -> Java_Byte,
      "char"                 -> CharRType(),
      "Char"                 -> CharRType(),
      "scala.Char"           -> CharRType(),
    //   "java.lang.Character"  -> Java_Char,
      "double"               -> DoubleRType(),
      "Double"               -> DoubleRType(),
      "scala.Double"         -> DoubleRType(),
    //   "java.lang.Double"     -> Java_Double,
      "float"                -> FloatRType(),
      "Float"                -> FloatRType(),
      "scala.Float"          -> FloatRType(),
    //   "java.lang.Float"      -> Java_Float,
      "int"                  -> IntRType(),
      "Int"                  -> IntRType(),
      "scala.Int"            -> IntRType(),
    //   "java.lang.Integer"    -> Java_Int,
      "long"                 -> LongRType(),
      "Long"                 -> LongRType(),
      "scala.Long"           -> LongRType(),
    //   "java.lang.Long"       -> Java_Long,
      "short"                -> ShortRType(),
      "Short"                -> ShortRType(),
      "scala.Short"          -> ShortRType(),
    //   "java.lang.Short"      -> Java_Short,
      "java.lang.String"     -> StringRType()
    //   "java.lang.Object"     -> Java_Object,
    //   "java.lang.Number"     -> Java_Number
    )