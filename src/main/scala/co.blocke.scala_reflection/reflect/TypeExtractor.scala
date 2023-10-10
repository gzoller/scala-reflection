package co.blocke.scala_reflection
package reflect

import scala.quoted.Quotes

trait TypeExtractor[T <: RTypeRef[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean

  def extractInfo[R](
      quotes: Quotes
  )(
      t: quotes.reflect.TypeRepr,
      tob: List[quotes.reflect.TypeRepr],
      symbol: quotes.reflect.Symbol
  )(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R]
