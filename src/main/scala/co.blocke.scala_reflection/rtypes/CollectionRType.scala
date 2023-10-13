package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

/** This RType mixin needed because all AppliedTypes don't have parameters.
  *  For examlpe a case class could be an applied type (isAppliedType=true) or not.  A collection is always applied.
  */
trait CollectionRType[R] extends AppliedRType:
  self: RType[?] =>

  val elementType: RType[?]
  def typeParamValues: List[RType[_]] = List(elementType)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val collectionType: quoted.Type[R] =
      quotes.reflect.TypeRepr.typeConstructorOf(self.clazz).asType.asInstanceOf[quoted.Type[R]]
    val elType: quoted.Type[elementType.T] = elementType.toType(quotes)
    val collectionTypeRepr = TypeRepr.of[R](using collectionType)
    val elTypeRepr = TypeRepr.of[elementType.T](using elType)
    AppliedType(collectionTypeRepr, List(elTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
