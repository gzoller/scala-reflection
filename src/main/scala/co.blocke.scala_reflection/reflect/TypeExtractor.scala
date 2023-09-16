package co.blocke.scala_reflection
package reflect

import scala.quoted.Quotes

trait TypeExtractor[T <: RType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean

  def extractInfo(quotes: Quotes)(
    t: quotes.reflect.TypeRepr, 
    tob: List[quotes.reflect.TypeRepr], 
    symbol: quotes.reflect.Symbol): RType[_]

