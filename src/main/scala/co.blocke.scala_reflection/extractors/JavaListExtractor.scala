package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes
import scala.util.Try

case class JavaListExtractor() extends TypeInfoExtractor[JavaListInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =  
    Try( Class.forName(symbol.fullName) <:< JListClazz ).toOption.getOrElse(false)

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 

    val clazz = Class.forName(symbol.fullName)
    val listElementType = tob.head
    val isTypeParam = listElementType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val listElementRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    JavaListInfo(
      clazz.getName, 
      listElementRType
    )

  def emptyInfo(clazz: Class[_]): JavaListInfo = 
    JavaListInfo(
      clazz.getName, 
      TypeSymbolInfo(clazz.getTypeParameters.toList.head.getName)
    )
