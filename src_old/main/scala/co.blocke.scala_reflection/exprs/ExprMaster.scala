package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

/*
Master object that creates all RType[T] Expr's.  Feed it an RType[T] and await the Expr[T].
When building Expr[T]s for classes there will likely be lots of recursion, so this is the
safe common entry point for whenever you need to descend into an RType to make an Expr.

This mechanism gets RTypes through the "blood-brain-barrier" between compile-type (quotes)
world, and the runtime (Expr) world.
 */

object ExprMaster:

  def makeExpr[T](rt: RType[T])(using q: Quotes)(using Type[T]): Expr[RType[T]] =
    rt match {
      case primitive: PrimitiveRType          => Primitives.makeExpr(primitive) // primitive types already preloaded in exprCache
      case clazz: ClassRType[T]               => Classes.makeExpr(clazz)
      case opt: OptionRType[T]                => Options.makeExpr(opt)
      case seq: SeqRType[T]                   => Seq.makeExpr(seq)
      case map: MapRType[T]                   => Map.makeExpr(map)
      case arr: ArrayRType[T]                 => Array.makeExpr(arr)
      case scalaTrait: TraitRType[T]          => Trait.makeExpr(scalaTrait)
      case sealedTrait: SealedTraitRType[T]   => Trait.makeExpr(sealedTrait)
      case enums: EnumRType[T]                => Enums.makeExpr(enums)
      case typeMember: TypeMemberRType        => TypeMember.makeExpr(typeMember)
      case typeSymbol: TypeSymbolRType[T]     => '{ TypeSymbolRType(${ Expr(typeSymbol.name) }).asInstanceOf[RType[T]] }
      case scalaEither: EitherRType[T]        => LeftRight.makeExpr(scalaEither)
      case scalaTry: TryRType[T]              => ScalaTry.makeExpr(scalaTry)
      case tuple: TupleRType[T]               => Tuple.makeExpr(tuple)
      case intersection: IntersectionRType[T] => LeftRight.makeExpr(intersection)
      case union: UnionRType[T]               => LeftRight.makeExpr(union)
      case alias: AliasRType[T]               => Alias.makeExpr(alias)
      case scala2: Scala2RType[T]             => '{ Scala2RType(${ Expr(scala2.name) }).asInstanceOf[RType[T]] }
      case javaList: JavaListRType[T]         => JavaList.makeExpr(javaList)
      case javaQueue: JavaQueueRType[T]       => JavaQueue.makeExpr(javaQueue)
      case javaStack: JavaStackRType[T]       => JavaStack.makeExpr(javaStack)
      case javaSet: JavaSetRType[T]           => JavaSet.makeExpr(javaSet)
      case javaMap: JavaMapRType[T]           => JavaMap.makeExpr(javaMap)
      case selfRef: SelfRefRType[T]           => SelfRef.makeExpr(selfRef)
      case obj: ObjectRType                   => '{ ObjectRType(${ Expr(obj.name) }).asInstanceOf[RType[T]] }
      case unknown: UnknownRType[T]           => '{ UnknownRType(${ Expr(unknown.name) }).asInstanceOf[RType[T]] }
    }
