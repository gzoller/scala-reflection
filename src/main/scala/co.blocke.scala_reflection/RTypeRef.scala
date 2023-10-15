package co.blocke.scala_reflection

import scala.quoted.*

/** Type Reference (Ref), which includes all the data ultimately needed to construct an RType[R].  Macros are defined by two sides of
  * the "blood-brain-barrier": compile-time and run-time.  At compile-time we know important data like Type[R] and others that are
  * derived from Quotes.  This data cannot survive the trip across the barrier to run-time, and frankly it's not needed over there.
  * Refs track Type and Expr data on the compile-side of the barrier.
  *
  * Refs have an expr val which is the Expr (expression) that will generate the associated RType on the run-time side of the
  * barrier.  The RType will be very similar but missing the compile-time data like Type.
  */
trait RTypeRef[R]:
  type T = R // R is saved for accessibility during casting, ie myRType.asInstanceOf[fooRType.T]
  val name: String
  val typedName: TypedName
  val expr: Expr[RType[R]]
  val refType: Type[R]
  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit
  def toRType: RType[R]
