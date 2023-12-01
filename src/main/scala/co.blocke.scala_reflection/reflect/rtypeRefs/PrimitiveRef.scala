package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.*
import rtypeRefs.*
import Clazzes.*
import util.{JsonField, JsonObjectBuilder}
import scala.math.{BigDecimal, BigInt}

/** Reference for all Java & Scala primitive types
  */

trait PrimitiveRef:
  val isNullable: Boolean

trait Stringish

case class BigDecimalRef()(using quotes: Quotes)(using tt: Type[BigDecimal]) extends RTypeRef[BigDecimal] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.BIG_DECIMAL_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[BigDecimalRType]), "<init>"),
      Nil
    ).asExprOf[RType[BigDecimal]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "BigDecimalRType"),
        JsonField("name", name)
      )
    )

case class BigIntRef()(using quotes: Quotes)(using tt: Type[BigInt]) extends RTypeRef[BigInt] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.BIG_INT_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[BigIntRType]), "<init>"),
      Nil
    ).asExprOf[RType[BigInt]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "BigIntRType"),
        JsonField("name", name)
      )
    )

case class BooleanRef()(using quotes: Quotes)(using tt: Type[Boolean]) extends RTypeRef[Boolean] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.BOOLEAN_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[BooleanRType]), "<init>"),
      Nil
    ).asExprOf[RType[Boolean]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "BooleanRType"),
        JsonField("name", name)
      )
    )

case class ByteRef()(using quotes: Quotes)(using tt: Type[Byte]) extends RTypeRef[Byte] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.BYTE_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[ByteRType]), "<init>"),
      Nil
    ).asExprOf[RType[Byte]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ByteRType"),
        JsonField("name", name)
      )
    )

case class CharRef()(using quotes: Quotes)(using tt: Type[Char]) extends RTypeRef[Char] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.CHAR_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[CharRType]), "<init>"),
      Nil
    ).asExprOf[RType[Char]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "CharRType"),
        JsonField("name", name)
      )
    )

case class DoubleRef()(using quotes: Quotes)(using tt: Type[Double]) extends RTypeRef[Double] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.DOUBLE_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[DoubleRType]), "<init>"),
      Nil
    ).asExprOf[RType[Double]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "DoubleRType"),
        JsonField("name", name)
      )
    )

case class FloatRef()(using quotes: Quotes)(using tt: Type[Float]) extends RTypeRef[Float] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.FLOAT_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[FloatRType]), "<init>"),
      Nil
    ).asExprOf[RType[Float]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "FloatRType"),
        JsonField("name", name)
      )
    )

case class IntRef()(using quotes: Quotes)(using tt: Type[Int]) extends RTypeRef[Int] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.INT_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[IntRType]), "<init>"),
      Nil
    ).asExprOf[RType[Int]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "IntRType"),
        JsonField("name", name)
      )
    )

case class LongRef()(using quotes: Quotes)(using tt: Type[Long]) extends RTypeRef[Long] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.LONG_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[LongRType]), "<init>"),
      Nil
    ).asExprOf[RType[Long]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "LongRType"),
        JsonField("name", name)
      )
    )

case class ShortRef()(using quotes: Quotes)(using tt: Type[Short]) extends RTypeRef[Short] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.SHORT_CLASS
  val typedName: TypedName = name
  override val isNullable = false
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[ShortRType]), "<init>"),
      Nil
    ).asExprOf[RType[Short]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ShortRType"),
        JsonField("name", name)
      )
    )

case class StringRef()(using quotes: Quotes)(using tt: Type[String]) extends RTypeRef[String] with PrimitiveRef with Stringish:
  import quotes.reflect.*
  val name = Clazzes.STRING_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[StringRType]), "<init>"),
      Nil
    ).asExprOf[RType[String]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "StringRType"),
        JsonField("name", name)
      )
    )

case class JBigDecimalRef()(using quotes: Quotes)(using tt: Type[java.math.BigDecimal]) extends RTypeRef[java.math.BigDecimal] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JBIG_DECIMAL_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaBigDecimalRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.math.BigDecimal]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaBigDecimalRType"),
        JsonField("name", name)
      )
    )

case class JBigIntegerRef()(using quotes: Quotes)(using tt: Type[java.math.BigInteger]) extends RTypeRef[java.math.BigInteger] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JBIG_INTEGER_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaBigIntegerRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.math.BigInteger]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaBigIntegerRType"),
        JsonField("name", name)
      )
    )

case class JBooleanRef()(using quotes: Quotes)(using tt: Type[java.lang.Boolean]) extends RTypeRef[java.lang.Boolean] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JBOOLEAN_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaBooleanRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Boolean]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaBooleanRType"),
        JsonField("name", name)
      )
    )

case class JByteRef()(using quotes: Quotes)(using tt: Type[java.lang.Byte]) extends RTypeRef[java.lang.Byte] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JBYTE_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaByteRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Byte]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaByteRType"),
        JsonField("name", name)
      )
    )

case class JCharacterRef()(using quotes: Quotes)(using tt: Type[java.lang.Character]) extends RTypeRef[java.lang.Character] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JCHARACTER_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaCharacterRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Character]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaCharacterRType"),
        JsonField("name", name)
      )
    )

case class JDoubleRef()(using quotes: Quotes)(using tt: Type[java.lang.Double]) extends RTypeRef[java.lang.Double] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JDOUBLE_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaDoubleRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Double]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaDoubleRType"),
        JsonField("name", name)
      )
    )

case class JFloatRef()(using quotes: Quotes)(using tt: Type[java.lang.Float]) extends RTypeRef[java.lang.Float] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JFLOAT_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaFloatRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Float]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaFloatRType"),
        JsonField("name", name)
      )
    )

case class JIntegerRef()(using quotes: Quotes)(using tt: Type[java.lang.Integer]) extends RTypeRef[java.lang.Integer] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JINTEGER_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaIntegerRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Integer]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaIntegerRType"),
        JsonField("name", name)
      )
    )

case class JLongRef()(using quotes: Quotes)(using tt: Type[java.lang.Long]) extends RTypeRef[java.lang.Long] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JLONG_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaLongRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Long]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaLongRType"),
        JsonField("name", name)
      )
    )

case class JShortRef()(using quotes: Quotes)(using tt: Type[java.lang.Short]) extends RTypeRef[java.lang.Short] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JSHORT_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaShortRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Short]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaShortRType"),
        JsonField("name", name)
      )
    )

case class JNumberRef()(using quotes: Quotes)(using tt: Type[java.lang.Number]) extends RTypeRef[java.lang.Number] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.JNUMBER_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaNumberRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Number]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaNumberRType"),
        JsonField("name", name)
      )
    )

case class AnyRef()(using quotes: Quotes)(using tt: Type[Any]) extends RTypeRef[Any] with PrimitiveRef:
  import quotes.reflect.*
  val name = Clazzes.ANY_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[AnyRType]), "<init>"),
      Nil
    ).asExprOf[RType[Any]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "AnyRType"),
        JsonField("name", name)
      )
    )

object PrimitiveRef:
  // Pre-bake primitive types w/cached builder functions
  // Note: AnyRef is NOT in the map, because it would match everything in ReflectOnType!
  protected[scala_reflection] val primTypeMap = Map(
    BIG_DECIMAL_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => BigDecimalRef()(using quotes)(using Type.of[scala.math.BigDecimal](using quotes)) },
    BIG_INT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => BigIntRef()(using quotes)(using Type.of[scala.math.BigInt](using quotes)) },
    BOOLEAN_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => BooleanRef()(using quotes)(using Type.of[Boolean](using quotes)) },
    BYTE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => ByteRef()(using quotes)(using Type.of[Byte](using quotes)) },
    CHAR_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => CharRef()(using quotes)(using Type.of[Char](using quotes)) },
    DOUBLE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => DoubleRef()(using quotes)(using Type.of[Double](using quotes)) },
    FLOAT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => FloatRef()(using quotes)(using Type.of[Float](using quotes)) },
    INT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => IntRef()(using quotes)(using Type.of[Int](using quotes)) },
    LONG_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => LongRef()(using quotes)(using Type.of[Long](using quotes)) },
    SHORT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => ShortRef()(using quotes)(using Type.of[Short](using quotes)) },
    STRING_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => StringRef()(using quotes)(using Type.of[String](using quotes)) },
    JBIG_DECIMAL_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JBigDecimalRef()(using quotes)(using Type.of[java.math.BigDecimal](using quotes)) },
    JBIG_INTEGER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JBigIntegerRef()(using quotes)(using Type.of[java.math.BigInteger](using quotes)) },
    JBOOLEAN_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JBooleanRef()(using quotes)(using Type.of[java.lang.Boolean](using quotes)) },
    JBYTE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JByteRef()(using quotes)(using Type.of[java.lang.Byte](using quotes)) },
    JCHARACTER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JCharacterRef()(using quotes)(using Type.of[java.lang.Character](using quotes)) },
    JDOUBLE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JDoubleRef()(using quotes)(using Type.of[java.lang.Double](using quotes)) },
    JFLOAT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JFloatRef()(using quotes)(using Type.of[java.lang.Float](using quotes)) },
    JINTEGER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JIntegerRef()(using quotes)(using Type.of[java.lang.Integer](using quotes)) },
    JLONG_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JLongRef()(using quotes)(using Type.of[java.lang.Long](using quotes)) },
    JSHORT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JShortRef()(using quotes)(using Type.of[java.lang.Short](using quotes)) },
    JNUMBER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JNumberRef()(using quotes)(using Type.of[java.lang.Number](using quotes)) },
    UUID_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => UUIDRef()(using quotes)(using Type.of[java.util.UUID](using quotes)) }
  )
