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

enum PrimFamily:
  case Stringish, Longish, Doublish, Boolish, Any
import PrimFamily.*

object PrimitiveRef:
  // Pre-bake primitive types w/cached builder functions
  protected[scala_reflection] val primFnMap = Map(
    BIG_DECIMAL_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[BigDecimal](BIG_DECIMAL_CLASS, Doublish, true)(using quotes)(using Type.of[BigDecimal](using quotes)) },
    BIG_INT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[BigInt](BIG_INT_CLASS, Longish, true)(using quotes)(using Type.of[BigInt](using quotes)) },
    BOOLEAN_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Boolean](BOOLEAN_CLASS, Boolish)(using quotes)(using Type.of[Boolean](using quotes)) },
    BYTE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Byte](BYTE_CLASS, Longish)(using quotes)(using Type.of[Byte](using quotes)) },
    CHAR_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Char](CHAR_CLASS, Stringish)(using quotes)(using Type.of[Char](using quotes)) },
    DOUBLE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Double](DOUBLE_CLASS, Doublish)(using quotes)(using Type.of[Double](using quotes)) },
    FLOAT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Float](FLOAT_CLASS, Doublish)(using quotes)(using Type.of[Float](using quotes)) },
    INT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Int](INT_CLASS, Longish)(using quotes)(using Type.of[Int](using quotes)) },
    LONG_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Long](LONG_CLASS, Longish)(using quotes)(using Type.of[Long](using quotes)) },
    SHORT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Short](SHORT_CLASS, Longish)(using quotes)(using Type.of[Short](using quotes)) },
    STRING_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[String](STRING_CLASS, Stringish, true)(using quotes)(using Type.of[String](using quotes)) },
    JBIG_DECIMAL_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.math.BigDecimal](JBIG_DECIMAL_CLASS, Doublish, true)(using quotes)(using Type.of[java.math.BigDecimal](using quotes)) },
    JBIG_INTEGER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.math.BigInteger](JBIG_INTEGER_CLASS, Longish, true)(using quotes)(using Type.of[java.math.BigInteger](using quotes)) },
    JBOOLEAN_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Boolean](JBOOLEAN_CLASS, Boolish, true)(using quotes)(using Type.of[java.lang.Boolean](using quotes)) },
    JBYTE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Byte](JBYTE_CLASS, Longish, true)(using quotes)(using Type.of[java.lang.Byte](using quotes)) },
    JCHARACTER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Character](JCHARACTER_CLASS, Stringish, true)(using quotes)(using Type.of[java.lang.Character](using quotes)) },
    JDOUBLE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Double](JDOUBLE_CLASS, Doublish, true)(using quotes)(using Type.of[java.lang.Double](using quotes)) },
    JFLOAT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Float](JFLOAT_CLASS, Doublish, true)(using quotes)(using Type.of[java.lang.Float](using quotes)) },
    JINTEGER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Integer](JINTEGER_CLASS, Longish, true)(using quotes)(using Type.of[java.lang.Integer](using quotes)) },
    JLONG_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Long](JLONG_CLASS, Longish, true)(using quotes)(using Type.of[java.lang.Long](using quotes)) },
    JSHORT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Short](JSHORT_CLASS, Longish, true)(using quotes)(using Type.of[java.lang.Short](using quotes)) },
    JOBJECT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Object](JOBJECT_CLASS, Stringish, true)(using quotes)(using Type.of[java.lang.Object](using quotes)) },
    JNUMBER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Number](JNUMBER_CLASS, Doublish, true)(using quotes)(using Type.of[java.lang.Number](using quotes)) },
    UUID_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.util.UUID](UUID_CLASS, Stringish, true)(using quotes)(using Type.of[java.util.UUID](using quotes)) }
  )

  private val primMap = Map(
    BIG_DECIMAL_CLASS -> classOf[BigDecimalRType],
    BIG_INT_CLASS -> classOf[BigIntRType],
    BOOLEAN_CLASS -> classOf[BooleanRType],
    BYTE_CLASS -> classOf[ByteRType],
    CHAR_CLASS -> classOf[CharRType],
    DOUBLE_CLASS -> classOf[DoubleRType],
    FLOAT_CLASS -> classOf[FloatRType],
    INT_CLASS -> classOf[IntRType],
    LONG_CLASS -> classOf[LongRType],
    SHORT_CLASS -> classOf[ShortRType],
    STRING_CLASS -> classOf[StringRType],
    ANY_CLASS -> classOf[AnyRType],
    JBIG_DECIMAL_CLASS -> classOf[JBigDecimalRType],
    JBIG_INTEGER_CLASS -> classOf[JBigIntegerRType],
    JBOOLEAN_CLASS -> classOf[JavaBooleanRType],
    JBYTE_CLASS -> classOf[JavaByteRType],
    JCHARACTER_CLASS -> classOf[JavaCharacterRType],
    JDOUBLE_CLASS -> classOf[JavaDoubleRType],
    JFLOAT_CLASS -> classOf[JavaFloatRType],
    JINTEGER_CLASS -> classOf[JavaIntegerRType],
    JLONG_CLASS -> classOf[JavaLongRType],
    JSHORT_CLASS -> classOf[JavaShortRType],
    JOBJECT_CLASS -> classOf[JavaObjectRType],
    JNUMBER_CLASS -> classOf[JavaNumberRType],
    UUID_CLASS -> classOf[UUIDRType]
  )

case class PrimitiveRef[R](
    name: String,
    family: PrimFamily,
    override val isNullable: Boolean = false
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName: TypedName = name
  val refType = tt

  type PrimType[X] = X match
    case BigDecimal           => BigDecimalRType
    case BigInt               => BigIntRType
    case Boolean              => BooleanRType
    case Byte                 => ByteRType
    case Char                 => CharRType
    case Double               => DoubleRType
    case Float                => FloatRType
    case Int                  => IntRType
    case Long                 => LongRType
    case Short                => ShortRType
    case String               => StringRType
    case Any                  => AnyRType
    case java.math.BigDecimal => JBigDecimalRType
    case java.math.BigInteger => JBigIntegerRType
    case java.lang.Boolean    => JavaBooleanRType
    case java.lang.Byte       => JavaByteRType
    case java.lang.Character  => JavaCharacterRType
    case java.lang.Double     => JavaDoubleRType
    case java.lang.Float      => JavaFloatRType
    case java.lang.Integer    => JavaIntegerRType
    case java.lang.Long       => JavaLongRType
    case java.lang.Short      => JavaShortRType
    case java.lang.Object     => JavaObjectRType
    case java.lang.Number     => JavaNumberRType
    case java.util.UUID       => UUIDRType

  val expr =
    val pt = TypeRepr.typeConstructorOf(PrimitiveRef.primMap(name))
    Apply(
      Select.unique(New(TypeTree.ref(pt.typeSymbol)), "<init>"),
      Nil
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "PrimitiveRType"),
        JsonField("name", name)
      )
    )
