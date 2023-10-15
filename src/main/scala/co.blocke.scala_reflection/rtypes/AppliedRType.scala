package co.blocke.scala_reflection
package rtypes

/** This RType mixin needed because all AppliedTypes don't have parameters.
  *  For examlpe a case class could be an applied type (isAppliedType=true) or not.  A collection is always applied.
  */
trait AppliedRType:
  self: RType[?] =>

  val typeParamSymbols: List[TypeSymbol]
  def typeParamValues: List[RType[_]]

  // Selecting is for creating and navigating paths through type parameters.  A Map[K,V] has a select limit of 2,
  // so select(0) gives you the Ref for K and select(1) gives the Ref for V.
  def selectLimit: Int
  def select(i: Int): RType[?]
