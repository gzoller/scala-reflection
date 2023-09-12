package co.blocke.scala_reflection
package impl


object Clazzes {
  // val EnumClazz        = classOf[Enum]
  val TryClazz         = Class.forName("scala.util.Try")
  val MapClazz         = Class.forName("scala.collection.Map")
  val SetClazz         = Class.forName("scala.collection.Set")
  val SeqClazz         = Class.forName("scala.collection.Seq")
  val OptionClazz      = Class.forName("scala.Option")
  val EitherClazz      = Class.forName("scala.util.Either")
  val BooleanClazz     = classOf[Boolean]
  val ByteClazz        = classOf[Byte]
  val CharClazz        = classOf[Char]
  val DoubleClazz      = classOf[Double]
  val FloatClazz       = classOf[Float]
  val IntClazz         = classOf[Int]
  val LongClazz        = classOf[Long]
  val ShortClazz       = classOf[Short]
  val StringClazz      = classOf[String] // shared Java/Scala
  val ScalaArrayClazz  = Class.forName("scala.Array")

  // Java-specific -- lots of wrapped/primitive type stuff going on
  val booleanClazz    = java.lang.Boolean.TYPE
  val JBooleanClazz   = Class.forName("java.lang.Boolean")
  val byteClazz       = java.lang.Byte.TYPE
  val JByteClazz      = Class.forName("java.lang.Byte")
  val charClazz       = java.lang.Character.TYPE
  val JCharacterClazz = Class.forName("java.lang.Character")
  val doubleClazz     = java.lang.Double.TYPE
  val JDoubleClazz    = Class.forName("java.lang.Double")
  val floatClazz      = java.lang.Float.TYPE
  val JFloatClazz     = Class.forName("java.lang.Float")
  val intClazz        = java.lang.Integer.TYPE
  val JIntegerClazz   = Class.forName("java.lang.Integer")
  val longClazz       = java.lang.Long.TYPE
  val JLongClazz      = Class.forName("java.lang.Long")
  val shortClazz      = java.lang.Short.TYPE
  val JShortClazz     = Class.forName("java.lang.Short")

  val AnyClazz        = classOf[Any]
  val ObjectClazz     = Class.forName("java.lang.Object")
  val ParamTypeClazz  = Class.forName("java.lang.reflect.ParameterizedType")
  val OptionalClazz   = Class.forName("java.util.Optional")
  val JMapClazz       = classOf[java.util.Map[_,_]]
  val JListClazz      = classOf[java.util.List[_]]
  val JQueueClazz     = classOf[java.util.Queue[_]]
  val JSetClazz       = classOf[java.util.Set[_]]
  val JStackClazz     = classOf[java.util.Stack[_]]
  val JNumberClazz    = Class.forName("java.lang.Number")

  // Class Ops
  extension (c: Class[_])
    def =:=(other: Class[_]): Boolean = c == other
    def <:<(other: Class[_]): Boolean = other.isAssignableFrom(c)
}