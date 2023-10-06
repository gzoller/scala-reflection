package co.blocke.scala_reflection
package extractors

import rtypes.*
import rtypes.Clazzes.*
import reflect.TypeExtractor
import scala.quoted.Quotes

case class TryExtractor() extends TypeExtractor[TryRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == TryClazz.getName

  def extractInfo(
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol): RType[?] =

    val tryOfType = tob.head
    val isTypeParam = tryOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val typeParamSymbols = List("A")
    val tryOfRType =
      if isTypeParam then TypeSymbolRType(tob.head.typeSymbol.name)
      else RType.unwindType(quotes)(tob.head)

    TryRType(
      t.classSymbol.get.fullName,
      typeParamSymbols,
      tryOfRType
    )
