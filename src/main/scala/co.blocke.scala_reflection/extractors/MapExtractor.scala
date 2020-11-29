package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes

case class MapExtractor() extends TypeInfoExtractor[MapLikeInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try( MapClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)


  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 

    val leftType = tob(0)
    val leftRType = 
      if leftType.typeSymbol.flags.is(quotes.reflect.Flags.Param) then
        TypeSymbolInfo(tob(0).typeSymbol.name)
      else
        RType.unwindType(quotes)(tob(0))

    val rightType = tob(1)
    val rightRType = 
      if rightType.typeSymbol.flags.is(quotes.reflect.Flags.Param) then
        TypeSymbolInfo(tob(1).typeSymbol.name)
      else
        RType.unwindType(quotes)(tob(1))

    MapLikeInfo(
      t.classSymbol.get.fullName,
      leftRType,
      rightRType
    )
