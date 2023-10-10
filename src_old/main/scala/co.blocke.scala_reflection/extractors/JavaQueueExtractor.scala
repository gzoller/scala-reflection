package co.blocke.scala_reflection
package extractors

import rtypes.*
import rtypes.Clazzes.*
import reflect.TypeExtractor
import scala.quoted.Quotes
import scala.util.Try

case class JavaQueueExtractor() extends TypeExtractor[JavaQueueRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    Try(Class.forName(symbol.fullName) <:< JQueueClazz).toOption.getOrElse(false)

  def extractInfo(
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol): RType[?] =

    val clazz = Class.forName(symbol.fullName)
    val elementType = tob.head
    val isTypeParam = elementType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val typeParamSymbols = List("A")
    val elementRType =
      if isTypeParam then TypeSymbolRType(tob.head.typeSymbol.name)
      else RType.unwindType(quotes)(tob.head)

    JavaQueueRType(
      clazz.getName,
      typeParamSymbols,
      elementRType
    )
