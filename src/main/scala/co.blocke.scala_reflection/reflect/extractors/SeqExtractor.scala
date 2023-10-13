package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import Clazzes.*
import reflect.TypeExtractor
import scala.quoted.*

case class SeqExtractor() extends TypeExtractor[SeqRef[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    val isSeq = scala.util.Try(SeqClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)
    val isSet = scala.util.Try(SetClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)
    isSeq || isSet

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val typeParamSymbols = List("A")
    val seqOfRef =
      tob.head.asType match
        case '[u] =>
          if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using Type.of[Any])
          else reflect.ReflectOnType[u](quotes)(tob.head)

    val a = quotes.reflect.AppliedType(t, tob).asType
    a match
      case '[t] =>
        SeqRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          seqOfRef
        ).asInstanceOf[RTypeRef[R]]
