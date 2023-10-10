package co.blocke.scala_reflection
package util

import scala.quoted.Quotes


object TypedName:

  // Need a full name inclusive of type parameters and correcting for Enumeration's class name erasure.
  // This name is used for RType.equals so caching works.
  def apply(quotes: Quotes)(aType: quotes.reflect.TypeRepr): TypedName =
    import quotes.reflect.*

    aType.asInstanceOf[TypeRef] match {
      case AppliedType(t, tob) =>
        t match {
          case AppliedType(t2, tob2) =>
            apply(quotes)(t2).toString + tob2
              .map(oneTob => apply(quotes)(oneTob.asInstanceOf[TypeRef]))
              .mkString("[", ",", "]")
          case _ =>
            apply(quotes)(t).toString + tob
              .map(oneTob => apply(quotes)(oneTob.asInstanceOf[TypeRef]))
              .mkString("[", ",", "]")
        }
      case sym if aType.typeSymbol.flags.is(Flags.Param) =>
        sym.name
      case AndType(left, right) =>
        Clazzes.INTERSECTION_CLASS + "[" + apply(quotes)(left.asInstanceOf[TypeRef]) + "," + apply(quotes)(
          right.asInstanceOf[TypeRef]
        ) + "]"
      case OrType(left, right) =>
        Clazzes.UNION_CLASS + "[" + apply(quotes)(left.asInstanceOf[TypeRef]) + "," + apply(quotes)(
          right.asInstanceOf[TypeRef]
        ) + "]"
      case _: dotty.tools.dotc.core.Types.WildcardType =>
        "unmapped"
      case _ =>
        aType.classSymbol.get.fullName match {
          case Clazzes.ENUMERATION_CLASS =>
            aType.asInstanceOf[TypeRef].qualifier.asInstanceOf[quotes.reflect.TermRef].termSymbol.moduleClass.fullName
          case tn =>
            tn
        }
    }