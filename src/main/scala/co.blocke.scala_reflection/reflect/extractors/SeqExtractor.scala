package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import Clazzes.*
import scala.quoted.*

object SeqType {
  type IsSeq[A <: scala.collection.Seq[?]] = A
  type IsSet[A <: scala.collection.Set[?]] = A
  type IsIterable[A <: scala.collection.Iterable[?]] = A
}

case class SeqExtractor() extends TypeExtractor[SeqRef[?]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    val isSeq = scala.util.Try(SeqClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)
    val isSet = scala.util.Try(SetClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)
    val isIterable = scala.util.Try(IterableClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)
    isSeq || isSet || isIterable

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes
    import quotes.reflect.*

    val typeParamSymbols = List("A")
    val seqOfRef = tob.head.dealias match
      case TypeBounds(low, high) => // Detect wildcards: List[? <: Thing]
        low.asType match
          case '[l] =>
            high.asType match
              case '[h] =>
                WildcardRef(
                  reflect.ReflectOnType[l](quotes)(low),
                  reflect.ReflectOnType[h](quotes)(high)
                )
      case _ =>
        tob.head.asType match
          case '[u] =>
            if tob.head.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob.head.typeSymbol.name)(using quotes)(using Type.of[Any])
            else reflect.ReflectOnType[u](quotes)(tob.head)

    quotes.reflect.AppliedType(t, tob).asType.asInstanceOf[Type[? <: Seq[?]]] match
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
      case '[SeqType.IsIterable[t]] =>
        IterableRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          seqOfRef
        ).asInstanceOf[RTypeRef[R]]
      case '[z] =>
        throw new Exception("Unknown seq type... " + Type.show[z])
