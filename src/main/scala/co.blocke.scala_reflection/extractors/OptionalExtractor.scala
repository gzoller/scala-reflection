package co.blocke.scala_reflection
package extractors

import rtypes.Clazzes.*
import rtypes.{JavaOptionalRType, TypeSymbolRType}
import reflect.TypeExtractor
import scala.quoted.Quotes
import scala.util.Try

case class OptionalExtractor() extends TypeExtractor[JavaOptionalRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =  
    Try( Class.forName(symbol.fullName) =:= OptionalClazz ).toOption.getOrElse(false)

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType[_] = 

    val clazz = Class.forName(symbol.fullName)
    val elementType = tob.head
    val isTypeParam = elementType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val typeParamSymbols = List("A")
    val elementRType = 
      if isTypeParam then
        TypeSymbolRType(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    JavaOptionalRType(
      clazz.getName, 
      typeParamSymbols,
      elementRType
    )
