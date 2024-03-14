package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import scala.annotation.tailrec
import rtypeRefs.*
import rtypes.*
import javax.management.ReflectionException

/** The goal of TypeSymbolFinder is to deep-dive through a class and find where in the type tree each type parameter is located.
  * For example:
  *
  *    case class Bar[T]( stuff: List[T])
  *    case class Foo[A,B]( thing: A, other: Bar[B] )
  *
  * We'd fiscover that A is found on the first argument of Foo, and that B is found by diving into Bar and ultimately List to find B.
  */
object TypeSymbolMapper:

  extension (list: List[TypeRecord])
    def indexOf2(elem: TypeSymbol): Int =
      list.indexWhere(_.typeSymbol == elem, 0)

  case class TypeRecord(
      typeSymbol: TypeSymbol,
      path: List[Int],
      isFound: Boolean = false
  ):
    def pushPath(i: Int, setFound: Boolean = false): TypeRecord =
      if !isFound then this.copy(path = path :+ i, isFound = setFound)
      else this
    def popPath(): TypeRecord =
      if !isFound then this.copy(path = path.dropRight(1))
      else this

  def mapTypeSymbolsForClass(
      quotes: Quotes
  )(classDef: quotes.reflect.ClassDef, paramSymbols: List[TypeSymbol]): Map[String, List[List[Int]]] =
    import quotes.reflect.*
    implicit val q = quotes
    val seenBefore = scala.collection.mutable.Map.empty[TypedName, Boolean]

    inline def isSealed(symbol: Symbol) = symbol.flags.is(quotes.reflect.Flags.Sealed)

    classDef.parents
      .map(_.asInstanceOf[TypeTree].tpe)
      .collect {
        case a: AppliedType if !isSealed(a.typeSymbol) => // for each AppliedType ancestor of this class...
          val extendsType =
            a.asType match
              case '[t] =>
                ReflectOnType[t](quotes)(a)(using seenBefore)
          val initialMap = paramSymbols.map(ts => TypeRecord(ts, Nil))
          val result = navLevel(extendsType, -1, initialMap) match {
            case Left(r) =>
              r // Doesn't matter if finished or not--return it.  The Left/Right was for navLevel internal abort-when-done
            case Right(r) =>
              r
          }
          (extendsType.name, result.map(_.path))
      }
      .toMap

  private def navLevel(
      rt: RTypeRef[?],
      index: Int,
      paramList: List[TypeRecord]
  ): Either[List[TypeRecord], List[TypeRecord]] = // Left => More to do, Right => complete
    rt match {
      case ap: AppliedRef =>
        val initialList =
          if index < 0 then paramList
          else paramList.map(_.pushPath(index)) // add current path to all un-found symbols

        // Special abortable foldLeft, so for large classes we don't have to recuse the entire deep class structure once we find all the types
        val afterAppliedList = foldLeftBreak((0 to ap.selectLimit - 1).toList)(initialList) { (i, pList) =>
          navLevel(ap.select(i), i, pList)
        }

        val cleanedList =
          afterAppliedList.map(_.popPath()) // revert any paths that weren't found in our deep dive into AppliedType
        var numLeftToFind = cleanedList.foldLeft(cleanedList.size) { (numLeft, rec) =>
          if rec.isFound then numLeft - 1 else numLeft
        }
        if numLeftToFind == 0 then Right(cleanedList)
        else Left(cleanedList)

      case ts: TypeSymbolRef =>
        paramList.indexOf2(ts.name.asInstanceOf[TypeSymbol]) match {
          case i if (i >= 0 && !paramList(i).isFound) =>
            Left(paramList.updated(i, paramList(i).pushPath(index, true)))
          case _ =>
            Left(paramList) // do nothing--not found, or already found
        }

      case _ => // another RType with no type parameter involvement, e.g. primitive type
        Left(paramList)
    }

  def runPath(quotes: Quotes)(path: List[List[Int]], traitTypeRepr: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    @tailrec
    def _runPath(onePath: List[Int], repr: TypeRepr): TypeRepr =
      if onePath == Nil then repr
      else
        val AppliedType(_, tob) = repr.asInstanceOf[AppliedType]
        _runPath(onePath.tail, tob(onePath.head))

    path.map(p => _runPath(p, traitTypeRepr))
