package co.blocke.scala_reflection
package rtypeRefs

import scala.quoted.*
import rtypes.*
import rtypeRefs.*
import Clazzes.*
import util.{JsonField, JsonObjectBuilder}

/** Reference for all Java & Scala primitive types
  */

object PrimitiveRef:
  // Pre-bake primitive types w/cached builder functions
  protected[scala_reflection] val primFnMap = Map(
    BOOLEAN_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Boolean](BOOLEAN_CLASS)(using quotes)(using Type.of[Boolean](using quotes)) },
    BYTE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Byte](BYTE_CLASS)(using quotes)(using Type.of[Byte](using quotes)) },
    CHAR_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Char](CHAR_CLASS)(using quotes)(using Type.of[Char](using quotes)) },
    DOUBLE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Double](DOUBLE_CLASS)(using quotes)(using Type.of[Double](using quotes)) },
    FLOAT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Float](FLOAT_CLASS)(using quotes)(using Type.of[Float](using quotes)) },
    INT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Int](INT_CLASS)(using quotes)(using Type.of[Int](using quotes)) },
    LONG_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Long](LONG_CLASS)(using quotes)(using Type.of[Long](using quotes)) },
    SHORT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[Short](SHORT_CLASS)(using quotes)(using Type.of[Short](using quotes)) },
    STRING_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[String](STRING_CLASS)(using quotes)(using Type.of[String](using quotes)) },
    JBOOLEAN_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Boolean](JBOOLEAN_CLASS)(using quotes)(using Type.of[java.lang.Boolean](using quotes)) },
    JBYTE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Byte](JBYTE_CLASS)(using quotes)(using Type.of[java.lang.Byte](using quotes)) },
    JCHARACTER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Character](JCHARACTER_CLASS)(using quotes)(using Type.of[java.lang.Character](using quotes)) },
    JDOUBLE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Double](JDOUBLE_CLASS)(using quotes)(using Type.of[java.lang.Double](using quotes)) },
    JFLOAT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Float](JFLOAT_CLASS)(using quotes)(using Type.of[java.lang.Float](using quotes)) },
    JINTEGER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Integer](JINTEGER_CLASS)(using quotes)(using Type.of[java.lang.Integer](using quotes)) },
    JLONG_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Long](JLONG_CLASS)(using quotes)(using Type.of[java.lang.Long](using quotes)) },
    JSHORT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Short](JSHORT_CLASS)(using quotes)(using Type.of[java.lang.Short](using quotes)) },
    JOBJECT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Object](JOBJECT_CLASS)(using quotes)(using Type.of[java.lang.Object](using quotes)) },
    JNUMBER_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PrimitiveRef[java.lang.Number](JNUMBER_CLASS)(using quotes)(using Type.of[java.lang.Number](using quotes)) }
  )

  private val primMap = Map(
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
    JBOOLEAN_CLASS -> classOf[JavaBooleanRType],
    JBYTE_CLASS -> classOf[JavaByteRType],
    JCHARACTER_CLASS -> classOf[JavaCharacterRType],
    JDOUBLE_CLASS -> classOf[JavaDoubleRType],
    JFLOAT_CLASS -> classOf[JavaFloatRType],
    JINTEGER_CLASS -> classOf[JavaIntegerRType],
    JLONG_CLASS -> classOf[JavaLongRType],
    JSHORT_CLASS -> classOf[JavaShortRType],
    JOBJECT_CLASS -> classOf[JavaObjectRType],
    JNUMBER_CLASS -> classOf[JavaNumberRType]
  )

case class PrimitiveRef[R](
    name: String
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName: TypedName = name
  val refType = tt

  type PrimType[X] = X match
    case Boolean             => BooleanRType
    case Byte                => ByteRType
    case Char                => CharRType
    case Double              => DoubleRType
    case Float               => FloatRType
    case Int                 => IntRType
    case Long                => LongRType
    case Short               => ShortRType
    case String              => StringRType
    case Any                 => AnyRType
    case java.lang.Boolean   => JavaBooleanRType
    case java.lang.Byte      => JavaByteRType
    case java.lang.Character => JavaCharacterRType
    case java.lang.Double    => JavaDoubleRType
    case java.lang.Float     => JavaFloatRType
    case java.lang.Integer   => JavaIntegerRType
    case java.lang.Long      => JavaLongRType
    case java.lang.Short     => JavaShortRType
    case java.lang.Object    => JavaObjectRType
    case java.lang.Number    => JavaNumberRType

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
