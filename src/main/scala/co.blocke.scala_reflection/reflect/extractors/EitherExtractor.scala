package co.blocke.scala_reflection
package reflect
package extractors

import rtypeRefs.*
import scala.quoted.*
import util.UniqueFinder

case class EitherExtractor() extends TypeExtractor[LeftRightRef[?]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = symbol.fullName == Clazzes.EitherClazz.getName

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes
    import quotes.reflect.*

    val typeParamSymbols = List("L", "R")

    val leftRef =
      tob(0).dealias match
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
          tob(0).asType match
            case '[u] =>
              if tob(0).typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob(0).typeSymbol.name)(using quotes)(using Type.of[Any])
              else reflect.ReflectOnType[u](quotes)(tob(0))
    val rightRef =
      tob(1).dealias match
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
          tob(1).asType match
            case '[u] =>
              if tob(1).typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(tob(1).typeSymbol.name)(using quotes)(using Type.of[Any])
              else reflect.ReflectOnType[u](quotes)(tob(1))

    val a = quotes.reflect.AppliedType(t, tob).asType
    a match
      case '[t] =>
        LeftRightRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          leftRef,
          rightRef,
          rtypeRefs.LRKind.EITHER,
          UniqueFinder.computeUniqueFields(leftRef, rightRef)
        ).asInstanceOf[RTypeRef[R]]
