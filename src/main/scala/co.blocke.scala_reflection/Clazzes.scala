package co.blocke.scala_reflection

import co.blocke.scala_reflection.rtypes.IntersectionRType

object Clazzes:

  // ======= Scala Class Names =======
  val BIG_DECIMAL_CLASS = "scala.math.BigDecimal"
  val BIG_INT_CLASS = "scala.math.BigInt"
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

  // ======= Java Class Names =======
  val JBIG_DECIMAL_CLASS = "java.math.BigDecimal"
  val JBIG_INTEGER_CLASS = "java.math.BigInteger"
  val JBOOLEAN_CLASS = "java.lang.Boolean"
  val JBYTE_CLASS = "java.lang.Byte"
  val JCHARACTER_CLASS = "java.lang.Character"
  val JDOUBLE_CLASS = "java.lang.Double"
  val JFLOAT_CLASS = "java.lang.Float"
  val JINTEGER_CLASS = "java.lang.Integer"
  val JLONG_CLASS = "java.lang.Long"
  val JSHORT_CLASS = "java.lang.Short"
  val JOBJECT_CLASS = "java.lang.Object"
  val JNUMBER_CLASS = "java.lang.Number"
  val UUID_CLASS = "java.util.UUID"

  /** Union and intersection types are only denoted internally as scala.Matchable, which is so generic it isn't helpful.
    *  These are synthetic marker class names to differentiate a union type.  Following Scala naming conventions, but be clear--there
    *  is (currently) no such class as "scala.Union" or "scala.Intersection" and bad things will happen if you try to
    *  instantiate one!
    */
  val UNION_CLASS = "scala.Union"
  val INTERSECTION_CLASS = "scala.Intersection"

  // ======= Class Instances =======
  val ArrayClazz = Class.forName("scala.Array")
  val EitherClazz = Class.forName("scala.util.Either")
  val JCollectionClazz = classOf[java.util.Collection[_]]
  val JMapClazz = classOf[java.util.Map[_, _]]
  val MapClazz = Class.forName("scala.collection.Map")
  val OptionalClazz = Class.forName("java.util.Optional")
  val OptionClazz = Class.forName("scala.Option")
  val SeqClazz = Class.forName("scala.collection.Seq")
  val SetClazz = Class.forName("scala.collection.Set")
  val TryClazz = Class.forName("scala.util.Try")

  val AnyClazz = classOf[Any]

  // Class Ops
  extension (c: Class[?])
    def =:=(other: Class[?]): Boolean = c == other
    def <:<(other: Class[?]): Boolean = other.isAssignableFrom(c)
