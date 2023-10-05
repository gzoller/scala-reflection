package co.blocke.scala_reflection
package extractors

import rtypes.*
import rtypes.Clazzes.*
import reflect.TypeExtractor
import scala.quoted.Quotes
import scala.util.Try

case class JavaStackExtractor() extends TypeExtractor[JavaStackRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =  
    Try( Class.forName(symbol.fullName) <:< JStackClazz ).toOption.getOrElse(false)

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType[_] = 

    val clazz = Class.forName(symbol.fullName)
    val listElementType = tob.head
    val isTypeParam = listElementType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val typeParamSymbols = List("A")
    val listElementRType = 
      if isTypeParam then
        TypeSymbolRType(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    JavaStackRType(
      clazz.getName, 
      typeParamSymbols,
      listElementRType
    )
