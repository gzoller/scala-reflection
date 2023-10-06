package co.blocke.scala_reflection
package extractors

import rtypes.Clazzes.*
import rtypes.{ScalaOptionRType, TypeSymbolRType}
import reflect.TypeExtractor
import scala.quoted.Quotes

case class OptionExtractor() extends TypeExtractor[ScalaOptionRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == OptionClazz.getName

  def extractInfo(
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol): RType[?] =

    val optionOfType = tob.head
    val isTypeParam = optionOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val typeParamSymbols = List("A")
    val optionOfRType =
      if isTypeParam then TypeSymbolRType(tob.head.typeSymbol.name)
      else RType.unwindType(quotes)(tob.head)

    ScalaOptionRType(
      t.classSymbol.get.fullName,
      typeParamSymbols,
      optionOfRType
    )
