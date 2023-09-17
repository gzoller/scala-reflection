package co.blocke.scala_reflection
package extractors

import rtypes.*
import rtypes.Clazzes.*
import reflect.TypeExtractor
import scala.quoted.Quotes

case class EitherExtractor() extends TypeExtractor[EitherRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == EitherClazz.getName


  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType[_] =

    val leftType = tob(0)
    val leftRType = 
      if leftType.typeSymbol.flags.is(quotes.reflect.Flags.Param) then
        TypeSymbolRType(tob(0).typeSymbol.name)
      else
        RType.unwindType(quotes)(tob(0))

    val rightType = tob(1)
    val rightRType = 
      if rightType.typeSymbol.flags.is(quotes.reflect.Flags.Param) then
        TypeSymbolRType(tob(1).typeSymbol.name)
      else
        RType.unwindType(quotes)(tob(1))

    EitherRType(
      t.classSymbol.get.fullName,
      leftRType,
      rightRType
    )
