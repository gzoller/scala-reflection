package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

/** Marker trait for all Scala/Java left/right types (either, intersection, union) */
trait LeftRightRType[R] extends AppliedRType:
  self: RType[R] =>

  lazy val leftType: RType[_]
  lazy val rightType: RType[_]

  val _leftType: RType[_]
  val _rightType: RType[_]

  def _copy( left: RType[_], right: RType[_] ): RType[_]

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val lrType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(self.clazz).asType.asInstanceOf[quoted.Type[R]]
    val leftParamType: quoted.Type[_leftType.T] = _leftType.toType(quotes)
    val rightParamType: quoted.Type[_rightType.T] = _rightType.toType(quotes)
    val lrTypeRepr = TypeRepr.of[R](using lrType)
    val leftParamTypeRepr = TypeRepr.of[_leftType.T](using leftParamType)
    val rightParamTypeRepr = TypeRepr.of[_rightType.T](using rightParamType)
    AppliedType(lrTypeRepr, List(leftParamTypeRepr, rightParamTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

  override def isAppliedType: Boolean = 
    (leftType, rightType) match {
      case (artL: AppliedRType, _) if artL.isAppliedType => true
      case (_, artR: AppliedRType) if artR.isAppliedType => true
      case _ => false
    }
    
  def selectLimit: Int = 2
  
  def select(i: Int): RType[_] = 
    i match {
      case 0 => leftType
      case 1 => rightType
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")
    }