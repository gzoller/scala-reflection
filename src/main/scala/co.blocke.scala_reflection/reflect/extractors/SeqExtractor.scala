package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import Clazzes.*
import reflect.TypeExtractor
import scala.quoted.*

object SeqType {
  type IsSeq[A <: scala.collection.Seq[_]] = A
  type IsSet[A <: scala.collection.Set[_]] = A
}

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

    val a = quotes.reflect.AppliedType(t, tob).asType.asInstanceOf[Type[? <: Seq[_]]]
    a match
      case '[SeqType.IsSeq[t]] =>
        SeqRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          seqOfRef
        ).asInstanceOf[RTypeRef[R]]
      case '[SeqType.IsSet[t]] =>
        SetRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          seqOfRef
        ).asInstanceOf[RTypeRef[R]]
      case '[z] =>
        throw new Exception("HEY!  Unknown type... " + Type.show[z])
