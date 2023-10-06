package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

/** RType for a Scala 2 class (no Tasty info)
  */
case class Scala2RType[R](name: String) extends RType[R]:
  val typedName: TypedName = name
  lazy val clazz: Class[?] = Class.forName(name)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "Scala2RType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
