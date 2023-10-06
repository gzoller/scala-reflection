package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
  */
trait EnumRType[T] extends RType[T]:
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

case class ScalaEnumRType[T](
    name: String,
    values: List[String]
) extends EnumRType[T]:
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

case class ScalaEnumerationRType[T](
    name: String,
    values: List[String]
) extends EnumRType[T]:
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

/*
object JavaEnumInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaEnumInfo =
    JavaEnumInfo(
      StringByteEngine.read(bbuf)
      )

      // When we get here: we can use class.getEnumConstants() to return array of T, the valid values of a Java enum
case class JavaEnumInfo protected[scala_reflection](
  name: String,
) extends RType:
  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
    val newTab = {if suppressIndent then tab else tab+1}
    {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName +s"(${infoClass.getName})\n"

  def toBytes( bbuf: ByteBuffer ): Unit =
    bbuf.put( JAVA_ENUM_INFO )
    StringByteEngine.write(bbuf, name)

  def jsSerialize(sb: StringBuffer): Unit =
    sb.append(s"""{"kind":"Java Enum","name":"$name","fullName":"$fullName"}""")
 */
