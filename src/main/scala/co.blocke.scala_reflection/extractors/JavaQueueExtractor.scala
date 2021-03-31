package co.blocke.scala_reflection
package extractors

import impl.*
import Clazzes.*
import info.* 
import scala.quoted.Quotes
import scala.util.Try

case class JavaQueueExtractor() extends TypeInfoExtractor[JavaQueueInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = 
    Try( Class.forName(symbol.fullName) <:< JQueueClazz ).toOption.getOrElse(false)

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

    JavaQueueInfo(
      clazz.getName, 
      elementRType
    )

  def emptyInfo(clazz: Class[_]): JavaQueueInfo = 
    JavaQueueInfo(
      clazz.getName, 
      TypeSymbolInfo(clazz.getTypeParameters.toList.head.getName)
    )
