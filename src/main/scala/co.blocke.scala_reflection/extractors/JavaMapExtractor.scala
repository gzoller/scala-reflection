package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes
import scala.util.Try

case class JavaMapExtractor() extends TypeInfoExtractor[JavaMapInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = 
    Try( Class.forName(symbol.fullName) <:< JMapClazz ).toOption.getOrElse(false)

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 
      val clazz = Class.forName(symbol.fullName)

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
  
      JavaMapInfo(
        clazz.getName,
        leftRType,
        rightRType
      )

  def emptyInfo(clazz: Class[_]): JavaMapInfo = 
    JavaMapInfo(
      clazz.getName, 
      TypeSymbolInfo(clazz.getTypeParameters.toList.apply(0).getName),
      TypeSymbolInfo(clazz.getTypeParameters.toList.apply(1).getName)
    )
