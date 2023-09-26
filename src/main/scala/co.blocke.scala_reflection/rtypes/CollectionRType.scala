package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

/** Marker trait for all Scala/Java collections */
trait CollectionRType[R] extends AppliedRType:
  self: RType[R] =>

  val _elementType: RType[_]
  lazy val elementType: RType[_]

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val collectionType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(self.clazz).asType.asInstanceOf[quoted.Type[R]]
    val elType: quoted.Type[_elementType.T] = _elementType.toType(quotes)
    val collectionTypeRepr = TypeRepr.of[R](using collectionType)
    val elTypeRepr = TypeRepr.of[_elementType.T](using elType)
    AppliedType(collectionTypeRepr, List(elTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

  def select(i: Int): RType[_] = 
    if i == 0 then
      elementType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")
