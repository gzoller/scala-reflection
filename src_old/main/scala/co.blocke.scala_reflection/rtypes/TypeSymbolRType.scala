package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

/** RType for an unassigned type symbol, e.g. Foo[T]
  */

case class TypeSymbolRType[T](name: String) extends RType[T]:
  val typedName = name.asInstanceOf[TypedName]
  lazy val clazz = Clazzes.ObjectClazz

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TypeSymbolRType"),
        JsonField("name", this.name)
      )
    )
