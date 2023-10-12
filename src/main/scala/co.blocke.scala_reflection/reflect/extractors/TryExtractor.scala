package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import Clazzes.*
import scala.quoted.*

case class TryExtractor() extends TypeExtractor[TryRef[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == TryClazz.getName

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val typeParamSymbols = List("A")
    val tryOfRef =
      tob.head.asType match
        case '[u] =>
          if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using Type.of[Any])
          else reflect.ReflectOnType[u](quotes)(tob.head, false)

    TryRef(
      t.classSymbol.get.fullName,
      typeParamSymbols,
      tryOfRef
    ).asInstanceOf[RTypeRef[R]]
