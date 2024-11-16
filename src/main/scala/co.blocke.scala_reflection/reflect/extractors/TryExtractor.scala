package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import Clazzes.*
import scala.quoted.*

case class TryExtractor() extends TypeExtractor[TryRef[?]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == TryClazz.getName

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes
    import quotes.reflect.*

    val typeParamSymbols = List("A")
    val tryOfRef = tob.head.dealias match
      case TypeBounds(low, high) => // Detect wildcards: List[? <: Thing]
        low.asType match
          case '[l] =>
            high.asType match
              case '[h] =>
                WildcardRef(
                  reflect.ReflectOnType[l](quotes)(low),
                  reflect.ReflectOnType[h](quotes)(high)
                )
      case _ =>
        tob.head.asType match
          case '[u] =>
            if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using Type.of[Any])
            else reflect.ReflectOnType[u](quotes)(tob.head)

    val a = quotes.reflect.AppliedType(t, tob).asType
    a match
      case '[t] =>
        TryRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          tryOfRef
        ).asInstanceOf[RTypeRef[R]]
