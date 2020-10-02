package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection

case class EitherExtractor() extends TypeInfoExtractor[EitherInfo]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = symbol.fullName == EitherClazz.getName


  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.Type], 
    symbol: reflect.Symbol): RType =

    val leftType = tob(0)
    val leftRType = 
      if leftType.typeSymbol.flags.is(reflect.Flags.Param) then
        TypeSymbolInfo(tob(0).typeSymbol.name)
      else
        RType.unwindType(reflect)(tob(0))

    val rightType = tob(1)
    val rightRType = 
      if rightType.typeSymbol.flags.is(reflect.Flags.Param) then
        TypeSymbolInfo(tob(1).typeSymbol.name)
      else
        RType.unwindType(reflect)(tob(1))

    val tparms = EitherClazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])
    EitherInfo(
      t.classSymbol.get.fullName,
      leftRType,
      rightRType
    )
