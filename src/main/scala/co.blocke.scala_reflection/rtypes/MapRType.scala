package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

/** Arity 2 Collections, Map flavors, basiclly */
case class MapRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?], // map key
    elementType2: RType[?] // map value
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name + "[" + elementType.typedName + "," + elementType2.typedName + "]"
  def typeParamValues: List[RType[?]] = List(elementType, elementType2)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val mapType: quoted.Type[R] = super.toType(quotes)
    val keyParamType: quoted.Type[elementType.T] = elementType.toType(quotes)
    val valueParamType: quoted.Type[elementType2.T] = elementType2.toType(quotes)
    val mapTypeRepr = TypeRepr.of[R](using mapType)
    val keyParamTypeRepr = TypeRepr.of[elementType.T](using keyParamType)
    val valueParamTypeRepr = TypeRepr.of[elementType2.T](using valueParamType)
    AppliedType(mapTypeRepr, List(keyParamTypeRepr, valueParamTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
