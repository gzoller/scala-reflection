package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes

case class TryExtractor() extends TypeInfoExtractor[TryInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == TryClazz.getName

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 

    val tryOfType = tob.head
    val isTypeParam = tryOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val tryOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    TryInfo(
      t.classSymbol.get.fullName,
      tryOfRType
    )
