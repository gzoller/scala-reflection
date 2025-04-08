package co.blocke.scala_reflection
package reflect
package extractors

import Clazzes.*
import rtypeRefs.*
import scala.quoted.*

object MapType {
  type IsMap[A <: scala.collection.Map[?, ?]] = A

  val insertionOrderMapTypes = Set(
    "scala.collection.immutable.ListMap",
    "scala.collection.immutable.LinkedHashMap", // Scala 2.13+
    "scala.collection.immutable.SeqMap",
    "scala.collection.mutable.LinkedHashMap",
    "scala.collection.mutable.ListMap",
    "scala.collection.mutable.SeqMap"
  )
}

case class MapExtractor() extends TypeExtractor[MapRef[?]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try(MapClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes
    import quotes.reflect.*

    val typeParamSymbols = List("K", "V")

    val elementRef =
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
    val elementRef2 =
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

    val a = quotes.reflect.AppliedType(t, tob)
    a.asType match
      case '[MapType.IsMap[t]] =>
        val typeName = a.tycon.typeSymbol.fullName
        val maintainsOrder = MapType.insertionOrderMapTypes.contains(typeName)
        MapRef[t](
          t.classSymbol.get.fullName,
          maintainsOrder,
          typeParamSymbols,
          elementRef,
          elementRef2
        ).asInstanceOf[RTypeRef[R]]
