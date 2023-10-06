package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class JavaListRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementType: RType[?]
) extends RType[R]
    with CollectionRType[R]:

  val typedName: TypedName = name + "[" + elementType.typedName + "]"
  def selectLimit: Int = 1

  lazy val clazz: Class[?] = Class.forName(name)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaListRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("elementType", this.elementType)
      )
    )
