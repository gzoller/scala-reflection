package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class ObjectRType(
    name: String
) extends RType[Object]:

  val typedName = name
  lazy val clazz: Class[?] = Class.forName(name)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ObjectRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
