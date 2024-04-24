package co.blocke.scala_reflection
package rtypes

/** This RType mixin needed because all AppliedTypes don't have parameters.
  *  For examlpe a case class could be an applied type (isAppliedType=true) or not.  A collection is always applied.
  */
trait AppliedRType:
  self: RType[?] =>

  val typeParamSymbols: List[TypeSymbol]
  def typeParamValues: List[RType[?]]
