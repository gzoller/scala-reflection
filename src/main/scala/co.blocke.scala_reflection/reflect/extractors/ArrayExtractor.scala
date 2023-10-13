package co.blocke.scala_reflection
package reflect
package extractors

import Clazzes.*
import rtypeRefs.*
import scala.quoted.*
import scala.util.Try

case class ArrayExtractor() extends TypeExtractor[ArrayRef[_]]:

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
      case p: PrimitiveRef[?] if p.name == BOOLEAN_CLASS => "Z"
      case p: PrimitiveRef[?] if p.name == BYTE_CLASS    => "B"
      case p: PrimitiveRef[?] if p.name == CHAR_CLASS    => "C"
      case p: PrimitiveRef[?] if p.name == DOUBLE_CLASS  => "D"
      case p: PrimitiveRef[?] if p.name == FLOAT_CLASS   => "F"
      case p: PrimitiveRef[?] if p.name == INT_CLASS     => "I"
      case p: PrimitiveRef[?] if p.name == LONG_CLASS    => "J"
      case p: PrimitiveRef[?] if p.name == SHORT_CLASS   => "S"
      case p: PrimitiveRef[?] if p.name == ANY_CLASS     => "Ljava.lang.Object;"
      case c                                             => "L" + c.name + ";"
    }
    "[" + mangled
