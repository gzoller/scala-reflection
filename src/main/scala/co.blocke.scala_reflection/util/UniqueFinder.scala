package co.blocke.scala_reflection
package util

import scala.collection.mutable
import reflect.rtypeRefs.*

object UniqueFinder:

  /**
   * This is used to determine if every child class of a sealed trait has a unique, non-optional field
   * that, if present, will uniquely identify the class. If even one child class has single unique field
   * then None is returned.
   */
  def uniqueFieldHashMap(classLikes: List[RTypeRef[?]]): Map[String, List[String]] =
    val flatClasses = flattenClassRefs(classLikes)
    flatClasses.groupMap { cls =>
      val hash = cls.fields
        .map(_.name)
        .sorted
        .mkString("#")
        .##.toString
      hash
    }(_.name)

  private def flattenClassRefs(classLikes: List[RTypeRef[?]]): List[ClassRef[?]] =
    classLikes.flatMap {
      case c: ClassRef[?] => List(c)
      case t: TraitRef[?] => flattenClassRefs(t.sealedChildren)
      case _ => Nil
    }

  val LEFT: Char = 'L'
  val RIGHT: Char = 'R'

  def computeUniqueFields(left: RTypeRef[?], right: RTypeRef[?]): Map[String, Char] =
    val leftHashes = collectHashes(left)
    val rightHashes = collectHashes(right)

    val uniqueLeft = leftHashes -- rightHashes
    val uniqueRight = rightHashes -- leftHashes

    (uniqueLeft.map(_ -> LEFT) ++ uniqueRight.map(_ -> RIGHT)).toMap

  private def collectHashes(ref: RTypeRef[?]): Set[String] =
    ref match
      case o: OptionRef[?] =>
        o.optionParamType match
          case c: ClassRef[?] => Set(hashOf(c.fields))
          case t: TraitRef[?] => t.sealedChildren.flatMap(collectHashes).toSet
          case lrr: LeftRightRef[?] => computeUniqueFields(lrr.leftRef, lrr.rightRef).keySet
          case _ => Set.empty

      case c: ClassRef[?] =>
        Set(hashOf(c.fields))

      case t: TraitRef[?] =>
        t.sealedChildren.flatMap(collectHashes).toSet

      case lrr: LeftRightRef[?] =>
        computeUniqueFields(lrr.leftRef, lrr.rightRef).keySet

      case _ =>
        Set.empty

  private def hashOf(fields: List[FieldInfoRef]): String =
    fields.map(_.name).sorted.mkString("#").##.toString
