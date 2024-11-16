package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import Clazzes.*
import scala.quoted.*
import scala.util.Try

case class JavaCollectionExtractor() extends TypeExtractor[JavaCollectionRef[?]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    Try(Class.forName(symbol.fullName) <:< JCollectionClazz).toOption.getOrElse(false)
      || symbol.fullName == "java.lang.Iterable"

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes
    import quotes.reflect.*

    val typeParamSymbols = List("A")
    val elementRef = tob.head.dealias match
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
        JavaCollectionRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          elementRef
        ).asInstanceOf[RTypeRef[R]]
