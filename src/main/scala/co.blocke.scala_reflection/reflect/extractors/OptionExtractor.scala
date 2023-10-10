package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.{ScalaOptionRef, TypeSymbolRef}
import reflect.TypeExtractor
import scala.quoted.*

case class OptionExtractor() extends TypeExtractor[ScalaOptionRef[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == Clazzes.OptionClazz.getName

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val typeParamSymbols = List("A")
    val optionOfRef =
      tob.head.asType match
        case '[u] =>
          if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then 
            TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using tob(0).asType.asInstanceOf[Type[u]])
          else
            reflect.ReflectOnType[u](quotes)(tob.head, false)

    ScalaOptionRef(
      t.classSymbol.get.fullName,
      typeParamSymbols,
      optionOfRef
    ).asInstanceOf[RTypeRef[R]]
