package co.blocke.scala_reflection
package extractors

import rtypes.*
import rtypes.Clazzes.*
import reflect.TypeExtractor
import scala.quoted.Quotes

case class SeqExtractor() extends TypeExtractor[SeqRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    val isSeq = scala.util.Try(SeqClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)
    val isSet = scala.util.Try(SetClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)
    isSeq || isSet

  def extractInfo(
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol): RType[?] =

    val listOfType = tob.head
    val isTypeParam = listOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val typeParamSymbols = List("A")
    val listOfRType =
      if isTypeParam then TypeSymbolRType(tob.head.typeSymbol.name)
      else RType.unwindType(quotes)(tob.head)

    SeqRType(
      t.classSymbol.get.fullName,
      typeParamSymbols,
      listOfRType
    )
