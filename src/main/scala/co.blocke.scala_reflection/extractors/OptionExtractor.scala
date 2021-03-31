package co.blocke.scala_reflection
package extractors

import impl.*
import Clazzes.*
import info.* 
import scala.quoted.Quotes

case class OptionExtractor() extends TypeInfoExtractor[ScalaOptionInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == OptionClazz.getName
  
  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 

    val optionOfType = tob.head
    val isTypeParam = optionOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val optionOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    ScalaOptionInfo(
      t.classSymbol.get.fullName, 
      optionOfRType
    )
