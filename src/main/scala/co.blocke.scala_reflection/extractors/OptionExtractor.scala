package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class OptionExtractor() extends TypeInfoExtractor[ScalaOptionInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == OptionClazz.getName

  
  def extractInfo(reflect: Reflection)(
    t: reflect.TypeRepr, 
    tob: List[reflect.TypeRepr], 
    symbol: reflect.Symbol): RType =

    val optionOfType = tob.head
    val isTypeParam = optionOfType.typeSymbol.flags.is(reflect.Flags.Param)
    val optionOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(reflect)(tob.head)

    ScalaOptionInfo(
      t.classSymbol.get.fullName, 
      optionOfRType
    )
