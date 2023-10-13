package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

trait LeftRightRType[R] extends AppliedRType:
  self: RType[?] =>

  val leftType: RType[?]
  val rightType: RType[?]

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val lrType: quoted.Type[R] =
      quotes.reflect.TypeRepr.typeConstructorOf(self.clazz).asType.asInstanceOf[quoted.Type[R]]
    val leftParamType: quoted.Type[leftType.T] = leftType.toType(quotes)
    val rightParamType: quoted.Type[rightType.T] = rightType.toType(quotes)
    val lrTypeRepr = TypeRepr.of[R](using lrType)
    val leftParamTypeRepr = TypeRepr.of[leftType.T](using leftParamType)
    val rightParamTypeRepr = TypeRepr.of[rightType.T](using rightParamType)
    AppliedType(lrTypeRepr, List(leftParamTypeRepr, rightParamTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
