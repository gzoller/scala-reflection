package co.blocke.scala_reflection
package reflect
package extractors

import Clazzes.*
import rtypeRefs.*
import scala.quoted.*
import scala.util.Try

case class ArrayExtractor() extends TypeExtractor[ArrayRef[?]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try(ArrayClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val typeParamSymbols = List("A")
    val elementRef =
      tob.head.asType match
        case '[u] =>
          if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using Type.of[Any])
          else reflect.ReflectOnType[u](quotes)(tob.head)

    val a = quotes.reflect.AppliedType(t, tob).asType
    a match
      case '[t] =>
        ArrayRef[t](
          mangleArrayClassName(elementRef),
          typeParamSymbols,
          elementRef
        ).asInstanceOf[RTypeRef[R]]

  private def mangleArrayClassName(ref: RTypeRef[?]): String =
    val mangled = ref match {
      case _: TypeSymbolRef => "Ljava.lang.Object;"
      case c: ArrayRef[?]   => mangleArrayClassName(c.elementRef)
      // case c: rtypes.JavaArrayInfo => mangleArrayClassName(c.elementType)
      case p: BooleanRef => "Z"
      case p: ByteRef    => "B"
      case p: CharRef    => "C"
      case p: DoubleRef  => "D"
      case p: FloatRef   => "F"
      case p: IntRef     => "I"
      case p: LongRef    => "J"
      case p: ShortRef   => "S"
      case p: AnyRef     => "Ljava.lang.Object;"
      case c             => "L" + c.name + ";"
    }
    "[" + mangled
