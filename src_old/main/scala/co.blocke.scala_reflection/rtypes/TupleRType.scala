package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class TupleRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    tupleTypes: List[RType[_]]
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name + tupleTypes.map(_.typedName).toList.mkString("[", ",", "]")
  def selectLimit: Int = tupleTypes.size

  lazy val clazz: Class[?] = Class.forName(name)

  override def isAppliedType: Boolean =
    tupleTypes
      .map {
        _ match {
          case artL: AppliedRType if artL.isAppliedType => true
          case _                                        => false
        }
      }
      .foldLeft(false)(_ | _)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val tupleType: quoted.Type[R] = super.toType(quotes)
    val tupleElementTypes: List[quoted.Type[_]] = tupleTypes.map(tt => tt.toType(quotes))
    val tupleTypeRepr = TypeRepr.of[R](using tupleType)
    val tupleElementTypeRepr = tupleTypes.zip(tupleElementTypes).map { case (rt, rtType) =>
      TypeRepr.of[rt.T](using rtType.asInstanceOf[quoted.Type[rt.T]])
    }
    AppliedType(tupleTypeRepr, tupleElementTypeRepr).asType.asInstanceOf[quoted.Type[R]]

  def select(i: Int): RType[?] =
    if i >= 0 && i <= tupleTypes.size - 1 then tupleTypes(i)
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TupleRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("tupleTypes", this.tupleTypes)
      )
    )
