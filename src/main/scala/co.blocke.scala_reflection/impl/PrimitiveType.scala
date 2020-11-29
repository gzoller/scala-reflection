package co.blocke.scala_reflection
package impl

import Clazzes._
import java.nio.ByteBuffer

enum PrimitiveType(val name: String, val _infoClass: Class[_], serialToken: Byte) extends RType:
  val fullName = name
  lazy val infoClass: Class[_] = _infoClass
  def toBytes( bbuf: ByteBuffer ): Unit = bbuf.put( serialToken )

  case Scala_Boolean extends PrimitiveType("scala.Boolean", BooleanClazz, SCALA_BOOLEAN)
  case Scala_Byte    extends PrimitiveType("scala.Byte", ByteClazz, SCALA_BYTE)
  case Scala_Char    extends PrimitiveType("scala.Char", CharClazz, SCALA_CHAR)
  case Scala_Double  extends PrimitiveType("scala.Double", DoubleClazz, SCALA_DOUBLE)
  case Scala_Float   extends PrimitiveType("scala.Float", FloatClazz, SCALA_FLOAT)
  case Scala_Int     extends PrimitiveType("scala.Int", IntClazz, SCALA_INT)
  case Scala_Long    extends PrimitiveType("scala.Long", LongClazz, SCALA_LONG)
  case Scala_Short   extends PrimitiveType("scala.Short", ShortClazz, SCALA_SHORT)
  case Scala_String  extends PrimitiveType("java.lang.String", StringClazz, SCALA_STRING)
  case Scala_Any     extends PrimitiveType("scala.Any", AnyClazz, SCALA_ANY)

  case Java_Boolean  extends PrimitiveType("java.lang.Boolean", JBooleanClazz, JAVA_BOOLEAN)
  case Java_Byte     extends PrimitiveType("java.lang.Byte", JByteClazz, JAVA_BYTE)
  case Java_Char     extends PrimitiveType("java.lang.Character", JCharacterClazz, JAVA_CHAR)
  case Java_Double   extends PrimitiveType("java.lang.Double", JDoubleClazz, JAVA_DOUBLE)
  case Java_Float    extends PrimitiveType("java.lang.Float", JFloatClazz, JAVA_FLOAT)
  case Java_Int      extends PrimitiveType("java.lang.Integer", JIntegerClazz, JAVA_INT)
  case Java_Long     extends PrimitiveType("java.lang.Long", JLongClazz, JAVA_LONG)
  case Java_Short    extends PrimitiveType("java.lang.Short", JShortClazz, JAVA_SHORT)
  case Java_Object   extends PrimitiveType("java.lang.Object", ObjectClazz, JAVA_OBJECT)
  case Java_Number   extends PrimitiveType("java.lang.Number", JNumberClazz, JAVA_NUMBER)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.name + "\n"


import PrimitiveType._
object PrimitiveType:
  def unapply(symName: String): Option[RType] = symName match {
    case "boolean"              => Some(Scala_Boolean)
    case "Boolean"              => Some(Scala_Boolean)
    case "scala.Boolean"        => Some(Scala_Boolean)
    case "java.lang.Boolean"    => Some(Java_Boolean)
    case "byte"                 => Some(Scala_Byte)
    case "Byte"                 => Some(Scala_Byte)
    case "scala.Byte"           => Some(Scala_Byte)
    case "java.lang.Byte"       => Some(Java_Byte)
    case "char"                 => Some(Scala_Char)
    case "Char"                 => Some(Scala_Char)
    case "scala.Char"           => Some(Scala_Char)
    case "java.lang.Character"  => Some(Java_Char)
    case "double"               => Some(Scala_Double)
    case "Double"               => Some(Scala_Double)
    case "scala.Double"         => Some(Scala_Double)
    case "java.lang.Double"     => Some(Java_Double)
    case "float"                => Some(Scala_Float)
    case "Float"                => Some(Scala_Float)
    case "scala.Float"          => Some(Scala_Float)
    case "java.lang.Float"      => Some(Java_Float)
    case "int"                  => Some(Scala_Int)
    case "Int"                  => Some(Scala_Int)
    case "scala.Int"            => Some(Scala_Int)
    case "java.lang.Integer"    => Some(Java_Int)
    case "long"                 => Some(Scala_Long)
    case "Long"                 => Some(Scala_Long)
    case "scala.Long"           => Some(Scala_Long)
    case "java.lang.Long"       => Some(Java_Long)
    case "short"                => Some(Scala_Short)
    case "Short"                => Some(Scala_Short)
    case "scala.Short"          => Some(Scala_Short)
    case "java.lang.Short"      => Some(Java_Short)
    case "java.lang.String"     => Some(Scala_String)
    case "java.lang.Object"     => Some(Java_Object)
    case "java.lang.Number"     => Some(Java_Number)
    case _ => None
  }

  def loadCache: Map[String, RType] = 
    Map(
      "boolean"              -> Scala_Boolean,
      "Boolean"              -> Scala_Boolean,
      "scala.Boolean"        -> Scala_Boolean,
      "java.lang.Boolean"    -> Java_Boolean,
      "byte"                 -> Scala_Byte,
      "Byte"                 -> Scala_Byte,
      "scala.Byte"           -> Scala_Byte,
      "java.lang.Byte"       -> Java_Byte,
      "char"                 -> Scala_Char,
      "Char"                 -> Scala_Char,
      "scala.Char"           -> Scala_Char,
      "java.lang.Character"  -> Java_Char,
      "double"               -> Scala_Double,
      "Double"               -> Scala_Double,
      "scala.Double"         -> Scala_Double,
      "java.lang.Double"     -> Java_Double,
      "float"                -> Scala_Float,
      "Float"                -> Scala_Float,
      "scala.Float"          -> Scala_Float,
      "java.lang.Float"      -> Java_Float,
      "int"                  -> Scala_Int,
      "Int"                  -> Scala_Int,
      "scala.Int"            -> Scala_Int,
      "java.lang.Integer"    -> Java_Int,
      "long"                 -> Scala_Long,
      "Long"                 -> Scala_Long,
      "scala.Long"           -> Scala_Long,
      "java.lang.Long"       -> Java_Long,
      "short"                -> Scala_Short,
      "Short"                -> Scala_Short,
      "scala.Short"          -> Scala_Short,
      "java.lang.Short"      -> Java_Short,
      "java.lang.String"     -> Scala_String,
      "java.lang.Object"     -> Java_Object,
      "java.lang.Number"     -> Java_Number
    )