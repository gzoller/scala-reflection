package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

case class TryRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    tryType: RType[?]
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name + "[" + tryType.typedName + "]"
  def typeParamValues: List[RType[?]] = List(tryType)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val thisTryType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[tryType.T] = tryType.toType(quotes)
    val tryTypeRepr = TypeRepr.of[R](using thisTryType)
    val paramTypeRepr = TypeRepr.of[tryType.T](using paramType)
    AppliedType(tryTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
