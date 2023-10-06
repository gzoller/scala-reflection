package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}
case class UnknownRType[T](name: String) extends RType[T]:
  val typedName = name.asInstanceOf[TypedName]
  lazy val clazz: Class[?] = Class.forName(name)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "UnknownRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
