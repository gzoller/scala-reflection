package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes

case class ScalaArrayExtractor() extends TypeInfoExtractor[ArrayInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =  
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try( ScalaArrayClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)


  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 

    val arrayOfType = tob.head
    val isTypeParam = arrayOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val arrayOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    val mangled = mangleArrayClassName(arrayOfRType)
    ArrayInfo(
      mangled,
      arrayOfRType)

