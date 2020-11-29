package co.blocke.scala_reflection
package extractors

import impl._
import Clazzes._
import info._ 
import scala.quoted.Quotes
import scala.util.matching.Regex

case class TupleExtractor() extends TypeInfoExtractor[TupleInfo]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = tupleFullName.matches(symbol.fullName)

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType = 

    val elementTypes = 
      tob.map{ oneTob =>
        if oneTob.typeSymbol.flags.is(quotes.reflect.Flags.Param) then
          TypeSymbolInfo(oneTob.typeSymbol.name)
        else
          RType.unwindType(quotes)(oneTob)
      }

    TupleInfo(t.classSymbol.get.fullName, elementTypes.toArray)

