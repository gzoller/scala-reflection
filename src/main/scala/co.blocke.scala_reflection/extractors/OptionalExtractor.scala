package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes
import scala.util.Try

case class OptionalExtractor() extends TypeInfoExtractor[JavaOptionalInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =  
    Try( Class.forName(symbol.fullName) =:= OptionalClazz ).toOption.getOrElse(false)

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 

    val clazz = Class.forName(symbol.fullName)
    val elementType = tob.head
    val isTypeParam = elementType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val elementRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    JavaOptionalInfo(
      clazz.getName, 
      elementRType
    )

  def emptyInfo(clazz: Class[_]): JavaOptionalInfo = 
    JavaOptionalInfo(
      clazz.getName,
      TypeSymbolInfo(clazz.getTypeParameters.toList.head.getName)
    )
