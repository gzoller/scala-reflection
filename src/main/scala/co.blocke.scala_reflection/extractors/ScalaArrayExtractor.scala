package co.blocke.scala_reflection
package extractors

import rtypes.Clazzes.*
import rtypes.{ArrayRType, TypeSymbolRType}
import reflect.TypeExtractor
import scala.quoted.Quotes

case class ScalaArrayExtractor() extends TypeExtractor[ArrayRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =  
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try( ScalaArrayClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)


  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType[_] = 

    val arrayOfType = tob.head
    val isTypeParam = arrayOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val arrayOfRType = 
      if isTypeParam then
        TypeSymbolRType(tob.head.typeSymbol.name)
      else
        RType.unwindType(quotes)(tob.head)

    val mangled = mangleArrayClassName(arrayOfRType)
    ArrayRType(
      mangled,
      arrayOfRType)

