package co.blocke.scala_reflection
package rtypes

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
  */
trait EnumRType[R] extends RType[R]:
  val values: List[String]
  def ordinal(v: String): Option[Int]
  def valueAt(i: Int): Option[String]

//---------------------------------------------------------< Scala 3 Enum

case class ScalaEnumRType[R](
    name: String,
    values: List[String]
) extends EnumRType[R]:
  val typedName: TypedName = name
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
    val clazz = Class.forName(name)
    val valuesMethod = clazz.getMethod("values")
    val valueSet = valuesMethod.invoke(clazz)
    valueSet
      .asInstanceOf[scala.collection.immutable.AbstractSet[Enumeration#Value]]
      .map(one => (one.toString, one.id))
      .toMap

  lazy val byId = byName.map(_.swap)

  def ordinal(v: String): Option[Int] = byName.get(v)
  def valueAt(i: Int): Option[String] = byId.get(i)

//---------------------------------------------------------< Java Enumeration

// When we get here: we can use class.getEnumConstants() to return array of T, the valid values of a Java enum
case class JavaEnumRType[R](
    name: String,
    values: List[String]
) extends EnumRType[R]:
  val typedName: TypedName = name

  def ordinal(v: String): Option[Int] = values.indexOf(v) match {
    case -1 => None
    case i  => Some(i)
  }
  def valueAt(i: Int): Option[String] =
    if i < 0 || i >= values.size then None
    else Some(values(i))
