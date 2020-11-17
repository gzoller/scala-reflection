package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.Try

case class JavaListExtractor() extends TypeInfoExtractor[JavaListInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = 
    Try( Class.forName(symbol.fullName) <:< JListClazz ).toOption.getOrElse(false)

  def extractInfo(reflect: Reflection)(
    t: reflect.TypeRepr, 
    tob: List[reflect.TypeRepr], 
    symbol: reflect.Symbol): RType = 
      val clazz = Class.forName(symbol.fullName)
      val listElementType = tob.head
      val isTypeParam = listElementType.typeSymbol.flags.is(reflect.Flags.Param)
      val listElementRType = 
        if isTypeParam then
          TypeSymbolInfo(tob.head.typeSymbol.name)
        else
          RType.unwindType(reflect)(tob.head)

      JavaListInfo(
        clazz.getName, 
        listElementRType
      )

  def emptyInfo(clazz: Class[_]): JavaListInfo = 
    JavaListInfo(
      clazz.getName, 
      TypeSymbolInfo(clazz.getTypeParameters.toList.head.getName)
    )
