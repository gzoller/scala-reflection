package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import Clazzes.*
import scala.quoted.*
import scala.util.Try

case class JavaQueueExtractor() extends TypeExtractor[JavaQueueRef[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    Try(Class.forName(symbol.fullName) <:< JQueueClazz).toOption.getOrElse(false)

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val typeParamSymbols = List("A")
    val elementRef =
      tob.head.asType match
        case '[u] =>
          if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using Type.of[Any])
          else reflect.ReflectOnType[u](quotes)(tob.head, false)

    JavaQueueRef(
      t.classSymbol.get.fullName,
      typeParamSymbols,
      elementRef
    ).asInstanceOf[RTypeRef[R]]
