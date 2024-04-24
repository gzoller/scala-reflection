package co.blocke.scala_reflection
package reflect
package extractors

import Clazzes.*
import rtypeRefs.{JavaOptionalRef, TypeSymbolRef}
import reflect.TypeExtractor
import scala.quoted.*
import scala.util.Try

case class JavaOptionalExtractor() extends TypeExtractor[JavaOptionalRef[?]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    Try(Class.forName(symbol.fullName) =:= OptionalClazz).toOption.getOrElse(false)

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val typeParamSymbols = List("A")
    val optionOfRef =
      tob.head.asType match
        case '[u] =>
          if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using Type.of[Any])
          else reflect.ReflectOnType[u](quotes)(tob.head)

    val a = quotes.reflect.AppliedType(t, tob).asType
    a match
      case '[t] =>
        JavaOptionalRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          optionOfRef
        ).asInstanceOf[RTypeRef[R]]
