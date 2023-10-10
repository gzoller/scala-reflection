package co.blocke.scala_reflection
package rtypeRefs

/** This RType mixin needed because all AppliedTypes don't have parameters.
  *  For examlpe a case class could be an applied type (isAppliedType=true) or not.  A collection is always applied.
  */
trait AppliedRef:
  self: RTypeRef[?] =>

  val typeParamSymbols: List[TypeSymbol]
  def isAppliedType: Boolean = true // can be overridden to false, e.g. Scala class that isn't parameterized

  // Selecting is for creating and navigating paths through type parameters.  A Map[K,V] has a select limit of 2,
  // so select(0) gives you the Ref for K and select(1) gives the Ref for V.
  def selectLimit: Int
  def select(i: Int): RTypeRef[?]
