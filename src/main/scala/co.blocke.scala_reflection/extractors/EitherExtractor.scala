package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes

case class EitherExtractor() extends TypeInfoExtractor[EitherInfo]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == EitherClazz.getName


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

    val tparms = EitherClazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
    EitherInfo(
      t.classSymbol.get.fullName,
      leftRType,
      rightRType
    )
