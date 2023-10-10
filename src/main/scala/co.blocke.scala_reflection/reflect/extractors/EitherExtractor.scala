package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.{LeftRightRef, TypeSymbolRef}
import scala.quoted.*

case class EitherExtractor() extends TypeExtractor[LeftRightRef[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == Clazzes.EitherClazz.getName

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val typeParamSymbols = List("L", "R")

    val leftRef =
      tob(0).asType match
        case '[t] =>
          if tob(0).typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob(0).typeSymbol.name)(using quotes)(using Type.of[Any])
          else reflect.ReflectOnType[t](quotes)(tob(0), false)

    val rightRef =
      tob(1).asType match
        case '[t] =>
          if tob(1).typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob(1).typeSymbol.name)(using quotes)(using Type.of[Any])
          else reflect.ReflectOnType[t](quotes)(tob(1), false)

    LeftRightRef(
      t.classSymbol.get.fullName,
      typeParamSymbols,
      leftRef,
      rightRef,
      rtypeRefs.LRKind.EITHER
    ).asInstanceOf[RTypeRef[R]]
