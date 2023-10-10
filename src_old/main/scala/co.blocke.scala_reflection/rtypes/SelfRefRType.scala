package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

/** Placeholder RType to be lazy-resolved, used for self-referencing types.  This is needed because without it, reflecting on
  *  a self-referencing type will enter an endless loop until the stack explodes.  This RType is immediately inserted into the
  *  type cache so that when the self-reference comes there's something in the cache to find.
  *  When one of these is encountered in the wild, just re-Reflect on the infoClass and you'll get the non-SelfRef (i.e. normal) RType
  */
case class SelfRefRType[R](name: String, typedName: TypedName) extends RType[R]:
  lazy val clazz: Class[?] = Class.forName(name)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "SelfRefRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName)
      )
    )
