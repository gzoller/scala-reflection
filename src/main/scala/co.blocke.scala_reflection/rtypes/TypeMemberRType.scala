package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class TypeMemberRType(
    name: String,
    typeSymbol: TypeSymbol,
    memberType: RType[?]
) extends RType[Any]:

  val typedName = name.asInstanceOf[TypedName]
  lazy val clazz: Class[?] = Clazzes.ObjectClazz

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TypeMemberRType"),
        JsonField("typeSymbol", this.typeSymbol),
        JsonField("memberType", this.memberType)
      )
    )
