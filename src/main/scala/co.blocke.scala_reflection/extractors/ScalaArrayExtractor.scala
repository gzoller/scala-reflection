package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class ScalaArrayExtractor() extends TypeInfoExtractor[ArrayInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try( ScalaArrayClazz.isAssignableFrom( Class.forName(symbol.fullName) ) ).toOption.getOrElse(false)


  def extractInfo(reflect: Reflection)(
    t: reflect.TypeRepr, 
    tob: List[reflect.TypeRepr], 
    symbol: reflect.Symbol): RType =

    val arrayOfType = tob.head
    val isTypeParam = arrayOfType.typeSymbol.flags.is(reflect.Flags.Param)
    val arrayOfRType = 
      if isTypeParam then
        TypeSymbolInfo(tob.head.typeSymbol.name)
      else
        RType.unwindType(reflect)(tob.head)

    val mangled = mangleArrayClassName(arrayOfRType)
    ArrayInfo(
      mangled,
      arrayOfRType)

