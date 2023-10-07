package co.blocke.scala_reflection
package rtypes

object Clazzes {

  // ======= Scala Class Names =======
  val BOOLEAN_CLASS = "scala.Boolean"
  val BYTE_CLASS = "scala.Byte"
  val CHAR_CLASS = "scala.Char"
  val DOUBLE_CLASS = "scala.Double"
  val FLOAT_CLASS = "scala.Float"
  val INT_CLASS = "scala.Int"
  val LONG_CLASS = "scala.Long"
  val SHORT_CLASS = "scala.Short"
  val STRING_CLASS = "java.lang.String"
  val ANY_CLASS = "scala.Any"
  val ENUMERATION_CLASS = "scala.Enumeration.Value"

  /** Union and intersection types are only denoted internally as scala.Matchable, which is so generic it isn't helpful.
    *  These are synthetic marker class names to differentiate a union type.  Following Scala naming conventions, but be clear--there
    *  is (currently) no such class as "scala.Union" or "scala.Intersection" and bad things will happen if you try to
    *  instantiate one!
    */
  val UNION_CLASS = "scala.Union"
  val INTERSECTION_CLASS = "scala.Intersection"

  // ======= Class Instances =======
  // val EnumClazz        = classOf[Enum]
  val TryClazz = Class.forName("scala.util.Try")
  val MapClazz = Class.forName("scala.collection.Map")
  val SetClazz = Class.forName("scala.collection.Set")
  val SeqClazz = Class.forName("scala.collection.Seq")
  val OptionClazz = Class.forName("scala.Option")
  val EitherClazz = Class.forName("scala.util.Either")
  val BooleanClazz = classOf[Boolean]
  val ByteClazz = classOf[Byte]
  val CharClazz = classOf[Char]
  val DoubleClazz = classOf[Double]
  val FloatClazz = classOf[Float]
  val IntClazz = classOf[Int]
  val LongClazz = classOf[Long]
  val ShortClazz = classOf[Short]
  val StringClazz = classOf[String] // shared Java/Scala
  val ScalaArrayClazz = Class.forName("scala.Array")

  // Java-specific -- lots of wrapped/primitive type stuff going on
  val booleanClazz = java.lang.Boolean.TYPE
  val JBooleanClazz = java.lang.Boolean.TYPE // Class.forName("java.lang.Boolean")
  val byteClazz = java.lang.Byte.TYPE
  val JByteClazz = Class.forName("java.lang.Byte")
  val charClazz = java.lang.Character.TYPE
  val JCharacterClazz = Class.forName("java.lang.Character")
  val doubleClazz = java.lang.Double.TYPE
  val JDoubleClazz = Class.forName("java.lang.Double")
  val floatClazz = java.lang.Float.TYPE
  val JFloatClazz = Class.forName("java.lang.Float")
  val intClazz = java.lang.Integer.TYPE
  val JIntegerClazz = Class.forName("java.lang.Integer")
  val longClazz = java.lang.Long.TYPE
  val JLongClazz = Class.forName("java.lang.Long")
  val shortClazz = java.lang.Short.TYPE
  val JShortClazz = Class.forName("java.lang.Short")

  val AnyClazz = classOf[Any]
  val ObjectClazz = Class.forName("java.lang.Object")
  val ParamTypeClazz = Class.forName("java.lang.reflect.ParameterizedType")
  val OptionalClazz = Class.forName("java.util.Optional")
  val JMapClazz = classOf[java.util.Map[_, _]]
  val JListClazz = classOf[java.util.List[_]]
  val JQueueClazz = classOf[java.util.Queue[_]]
  val JSetClazz = classOf[java.util.Set[_]]
  val JStackClazz = classOf[java.util.Stack[_]]
  val JEnumClazz = classOf[java.lang.Enum[_]]
  val JNumberClazz = Class.forName("java.lang.Number")

  // Class Ops
  extension (c: Class[?])
    def =:=(other: Class[?]): Boolean = c == other
    def <:<(other: Class[?]): Boolean = other.isAssignableFrom(c)
}
