package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import rtypeRefs.*

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
  )(classDef: quotes.reflect.ClassDef, paramSymbols: List[TypeSymbol])(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): Map[String, List[List[Int]]] =
    import quotes.reflect.*

    implicit val q = quotes
    classDef.parents
      .map(_.asInstanceOf[TypeTree].tpe)
      .collect { case a: AppliedType => // for each AppliedType ancestor of this class...
        val extendsType =
          a.asType match
            case '[t] =>
              reflect.ReflectOnType[t](quotes)(a)
        val initialMap = paramSymbols.map(ts => TypeRecord(ts, Nil))
        val result = navLevel(extendsType, -1, initialMap) match {
          case Left(r) =>
            r // Doesn't matter if finished or not--return it.  The Left/Right was for navLevel internal abort-when-done
          case Right(r) => r
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

  // Use paths in classRT to nav to particular spots in T's (trait) type param structure.
  // T will be a fully-typed trait (w/concrete types, not type symbols).  The goal is to use the
  // type map to figure out what the concrete Types for each of class's type parameters should be,
  // per the given trait.
  def deepApply(classRT: ScalaClassRef[?], traitRT: TraitRef[?])(using q: Quotes): List[q.reflect.TypeRepr] =
    import q.reflect.*

    // This nonsense is required because if you have type parameters with a union or intersection, wrapped in some
    // other AppliedType (like List[Int|String]), the And/Or types get demoted to Object, which blows up.
    // Need to rebuild the TypeRepr tree with the proper AndType/OrType.  <sigh>
    def rebuildAppliedType(rt: RTypeRef[?]): TypeRepr =
      rt match {
        case u: LeftRightRef[?] if u.lrkind == LRKind.UNION =>
          OrType(rebuildAppliedType(u.leftRef), rebuildAppliedType(u.rightRef))

        case i: LeftRightRef[?] if i.lrkind == LRKind.INTERSECTION =>
          AndType(rebuildAppliedType(i.leftRef), rebuildAppliedType(i.rightRef))

        case a: AppliedRef =>
          a.refType match
            case '[u] =>
              TypeRepr.of[u] match {
                case AppliedType(t, tob) =>
                  val paramReprs = (0 to a.selectLimit - 1).map(i => rebuildAppliedType(a.select(i))).toList
                  AppliedType(t, paramReprs)
              }

        case _ =>
          rt.refType match
            case '[t] =>
              TypeRepr.of[t]
      }

    def runPath(path: List[Int], rt: RTypeRef[?]): TypeRepr =
      rt match {
        case x if !x.isInstanceOf[AppliedRef] =>
          rt.refType match
            case '[t] =>
              TypeRepr.of[t]
        case a: AppliedRef =>
          path match {
            case Nil =>
              TypeRepr.of[Any] // If we can't map a type parameter, we default to Any-type
            case p :: rest =>
              val cur = a.select(p)
              if rest == Nil then rebuildAppliedType(cur)
              else runPath(rest, cur)
          }
      }

    val selectedParentMap = classRT.typeParamPaths.getOrElse(traitRT.name, classRT.typeParamSymbols.map(_ => Nil))
    selectedParentMap.map(onePath => runPath(onePath, traitRT))
