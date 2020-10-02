package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.tasty.Reflection
import scala.util.matching.Regex

case class TupleExtractor() extends TypeInfoExtractor[TupleInfo]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean = tupleFullName.matches(symbol.fullName)


  def extractInfo(reflect: Reflection)(
    t: reflect.Type, 
    tob: List[reflect.Type], 
    symbol: reflect.Symbol): RType =

    val elementTypes = 
      tob.map{ oneTob =>
        if oneTob.typeSymbol.flags.is(reflect.Flags.Param) then
          TypeSymbolInfo(oneTob.typeSymbol.name)
        else
          RType.unwindType(reflect)(oneTob)
      }

    TupleInfo(t.classSymbol.get.fullName, elementTypes.toArray)

