package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
  */
trait EnumRType[R] extends RType[R]:
  lazy val clazz: Class[?]
  val values: List[String]

  def ordinal(v: String): Option[Int]
  def valueAt(i: Int): Option[String]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", Show.lastPart(this.getClass.getName)),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("values", this.values)
      )
    )

//---------------------------------------------------------< Scala 3 Enum

case class ScalaEnumRType[R](
    name: String,
    values: List[String]
) extends EnumRType[R]:
  val typedName: TypedName = name
  lazy val clazz: Class[?] = Class.forName(name)
  def ordinal(v: String): Option[Int] = values.indexOf(v) match {
    case -1 => None
    case i  => Some(i)
  }
  def valueAt(i: Int): Option[String] =
    if i < 0 || i >= values.size then None
    else Some(values(i))

//---------------------------------------------------------< Scala 2 Enumeration

case class ScalaEnumerationRType[R](
    name: String,
    values: List[String]
) extends EnumRType[R]:
  val typedName: TypedName = name

  lazy val byName: Map[String, Int] =
    // <sigh>  We actually have to instantiate the Scala 2 Enumeration to extract all the goodies from it...
    val valuesMethod = clazz.getMethod("values")
    val valueSet = valuesMethod.invoke(clazz)
    valueSet
      .asInstanceOf[scala.collection.immutable.AbstractSet[Enumeration#Value]]
      .map(one => (one.toString, one.id))
      .toMap

  lazy val byId = byName.map(_.swap)

  lazy val clazz: Class[?] = Class.forName(name)
  def ordinal(v: String): Option[Int] = byName.get(v)
  def valueAt(i: Int): Option[String] = byId.get(i)

//---------------------------------------------------------< Java Enumeration

// When we get here: we can use class.getEnumConstants() to return array of T, the valid values of a Java enum
case class JavaEnumRType[R](
    name: String,
    values: List[String],
    expr: Option[scala.quoted.Expr[R]] = None // Internal use only! (fixes broken Classloader for Java classes inside a macro)
) extends EnumRType[R]:
  val typedName: TypedName = name
  lazy val clazz: Class[?] = Class.forName(name)

  def ordinal(v: String): Option[Int] = values.indexOf(v) match {
    case -1 => None
    case i  => Some(i)
  }
  def valueAt(i: Int): Option[String] =
    if i < 0 || i >= values.size then None
    else Some(values(i))

  override def toType(quotes: Quotes): quoted.Type[R] =
    quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]]
