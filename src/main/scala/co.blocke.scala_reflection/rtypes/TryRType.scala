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
  def typeParamValues: List[RType[_]] = List(tryType)

  val selectLimit: Int = 1

  def select(i: Int): RType[?] =
    if i == 0 then tryType
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val thisTryType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[tryType.T] = tryType.toType(quotes)
    val tryTypeRepr = TypeRepr.of[R](using thisTryType)
    val paramTypeRepr = TypeRepr.of[tryType.T](using paramType)
    AppliedType(tryTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
