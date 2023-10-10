package co.blocke.scala_reflection
package rtypeRefs

import scala.quoted.*
import rtypes.*
import util.{JsonField, JsonObjectBuilder}

/** Reference for all Java & Scala primitive types
  */
case class PrimitiveRef[R](
    name: String
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]:
  import quotes.reflect.*

  val typedName: TypedName = name
  val refType = tt

  type PrimType[X] = X match
    case String => StringRType
    case Int    => IntRType
    case Long   => LongRType
    case Any    => AnyRType

  val expr = {
    val pt = TypeRepr.typeConstructorOf(PrimitiveRef.primMap(name)).asType.asInstanceOf[Type[PrimType[R]]]
    Apply(
      Select.unique(New(TypeTree.of[PrimType[R]](using pt)), "<init>"),
      Nil
    ).asExprOf[RType[R]]
  }

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "PrimitiveRType"),
        JsonField("name", name)
      )
    )

object PrimitiveRef:
  import Clazzes.*

  private val primMap = Map(
    INT_CLASS -> classOf[IntRType],
    LONG_CLASS -> classOf[LongRType],
    STRING_CLASS -> classOf[StringRType],
    ANY_CLASS -> classOf[AnyRType]
  )
