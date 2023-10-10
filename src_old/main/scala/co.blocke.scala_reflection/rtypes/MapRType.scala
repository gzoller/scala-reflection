package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

/** Arity 2 Collections, Map flavors, basiclly */
case class MapRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?], // map key
    elementType2: RType[?] // map value
) extends RType[R]
    with CollectionRType[R]:

  val typedName: TypedName = name + "[" + elementType.typedName + "," + elementType2.typedName + "]"
  def selectLimit: Int = 2

  lazy val clazz: Class[?] = Class.forName(name)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val mapType: quoted.Type[R] = super.toType(quotes)
    val keyParamType: quoted.Type[elementType.T] = elementType.toType(quotes)
    val valueParamType: quoted.Type[elementType2.T] = elementType2.toType(quotes)
    val mapTypeRepr = TypeRepr.of[R](using mapType)
    val keyParamTypeRepr = TypeRepr.of[elementType.T](using keyParamType)
    val valueParamTypeRepr = TypeRepr.of[elementType2.T](using valueParamType)
    AppliedType(mapTypeRepr, List(keyParamTypeRepr, valueParamTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

  override def select(i: Int): RType[?] =
    i match {
      case 0 => elementType
      case 1 => elementType2
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for $name")
    }

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "MapRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("elementType", this.elementType),
        JsonField("elementType2", this.elementType2)
      )
    )
