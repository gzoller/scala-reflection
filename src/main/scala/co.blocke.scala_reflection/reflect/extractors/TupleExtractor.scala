package co.blocke.scala_reflection
package reflect
package extractors

import Clazzes.*
import rtypeRefs.*
import scala.quoted.*
import scala.util.matching.Regex

case class TupleExtractor() extends TypeExtractor[TupleRef[?]]:

  val tupleFullName: Regex = """scala.Tuple(\d+)""".r

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean = tupleFullName.matches(symbol.fullName)

  def extractInfo[R](
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[R] =
    implicit val q = quotes

    val elementTypes =
      tob.map { oneTob =>
        oneTob.asType match
          case '[u] =>
            if oneTob.typeSymbol.flags.is(quotes.reflect.Flags.Param) then TypeSymbolRef(oneTob.typeSymbol.name)(using quotes)(using Type.of[Any])
            else reflect.ReflectOnType[u](quotes)(oneTob)
      }
    val (_, typeParamSymbols) = elementTypes.foldLeft(('A', List.empty[String])) { case ((sym, acc), b) =>
      ((sym + 1).toChar, acc :+ sym.toString())
    }

    val a = quotes.reflect.AppliedType(t, tob).asType
    a match
      case '[t] =>
        TupleRef[t](
          t.classSymbol.get.fullName,
          typeParamSymbols,
          elementTypes
        ).asInstanceOf[RTypeRef[R]]
