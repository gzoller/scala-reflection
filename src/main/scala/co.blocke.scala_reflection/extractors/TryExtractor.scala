package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class TryExtractor() extends TypeInfoExtractor[TryInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == TryClazz.getName


  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.Type], 
    symbol: reflect.Symbol): RType =

    val tryOfType = tob.head
    val isTypeParam = tryOfType.typeSymbol.flags.is(reflect.Flags.Param)
    val tryOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(reflect)(tob.head)

    TryInfo(
      t.classSymbol.get.fullName,
      tryOfRType
    )
