package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

case class IntersectionRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _leftType: RType[_],
  _rightType: RType[_]
  ) extends RType[R] with LeftRightRType[R]:

    val typedName: TypedName = name + "[" + _leftType.typedName + "," + _rightType.typedName + "]"

    lazy val clazz: Class[_] = Clazzes.AnyClazz  // The only "class" And and Or types have is the useless Matchable class

    lazy val leftType: RType[_] = _leftType match {
      // case e: SelfRefRType => e.resolve
      case e => e
    }
    lazy val rightType: RType[_] = _rightType match {
      // case e: SelfRefRType => e.resolve
      case e => e
    } 

    def _copy( left: RType[_], right: RType[_] ) = this.copy(_leftType = left, _rightType = right)
