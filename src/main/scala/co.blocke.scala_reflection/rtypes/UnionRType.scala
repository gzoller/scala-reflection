package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes


case class UnionRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _leftType: RType[_],
  _rightType: RType[_]
  ) extends RType[R] with LeftRightRType[R]:

  val typedName: TypedName = name + "[" + _leftType.typedName + "," + _rightType.typedName + "]"

  lazy val clazz: Class[_] = Clazzes.AnyClazz  // something here so we don't blow up

  lazy val leftType: RType[_] = _leftType
  lazy val rightType: RType[_] = _rightType
  
  def _copy( left: RType[_], right: RType[_] ) = this.copy(_leftType = left, _rightType = right)

