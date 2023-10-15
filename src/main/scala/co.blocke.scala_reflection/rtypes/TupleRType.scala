package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
case class TupleRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RType[_]]
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name + typeParamValues.map(_.typedName).toList.mkString("[", ",", "]")

  val selectLimit: Int = typeParamValues.size

  def select(i: Int): RType[?] =
    if i >= 0 && i <= typeParamValues.size - 1 then typeParamValues(i)
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val tupleType: quoted.Type[R] = super.toType(quotes)
    val tupleElementTypes: List[quoted.Type[_]] = typeParamValues.map(tt => tt.toType(quotes))
    val tupleTypeRepr = TypeRepr.of[R](using tupleType)
    val tupleElementTypeRepr = typeParamValues.zip(tupleElementTypes).map { case (rt, rtType) =>
      TypeRepr.of[rt.T](using rtType.asInstanceOf[quoted.Type[rt.T]])
    }
    AppliedType(tupleTypeRepr, tupleElementTypeRepr).asType.asInstanceOf[quoted.Type[R]]
