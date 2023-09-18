package co.blocke.scala_reflection
package rtypes

case class EitherRType[R](
  name: String,
  _leftType: RType[_],
  _rightType: RType[_]
) extends RType[R] with LeftRightRType[R]: 

  val typedName: TypedName = name + "[" + _leftType.typedName + "," + _rightType.typedName + "]"

  lazy val clazz: Class[_] = Class.forName(name)

  lazy val leftType: RType[_] = _leftType match {
    // case e: SelfRefRType[_] => e.resolve
    case e => e
  }
  lazy val rightType: RType[_] = _rightType match {
    // case e: SelfRefRType[_] => e.resolve
    case e => e
  }

  def _copy( left: RType[_], right: RType[_] ) = EitherRType(name, left, right)
