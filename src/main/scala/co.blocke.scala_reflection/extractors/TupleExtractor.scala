package co.blocke.scala_reflection
package extractors

import rtypes.Clazzes.*
import rtypes.* 
import scala.quoted.Quotes
import reflect.TypeExtractor
import scala.util.matching.Regex

case class TupleExtractor() extends TypeExtractor[TupleRType[_]]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = tupleFullName.matches(symbol.fullName)

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType[_] = 

    val elementTypes = 
      tob.map{ oneTob =>
        if oneTob.typeSymbol.flags.is(quotes.reflect.Flags.Param) then
          TypeSymbolRType(oneTob.typeSymbol.name)
        else
          RType.unwindType(quotes)(oneTob)
      }

    TupleRType(t.classSymbol.get.fullName, elementTypes)

