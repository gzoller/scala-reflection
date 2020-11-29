package co.blocke.scala_reflection
package info

import java.nio.ByteBuffer
import impl._

/** Something to smooth the differences between the 2.x Enumeration class and the 3.x Enum class
 */
trait EnumInfo extends RType:
  lazy val infoClass: Class[_]
  val values: Array[String]
  def ordinal(s: String): Int
  def valueOf(s: String): Any
  def valueOf(i: Int): Any
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) with values [${values.map(_.toString).mkString(",")}]\n"

//---------------------------------------------------------

object ScalaEnumInfo:
  def fromBytes( bbuf: ByteBuffer ): ScalaEnumInfo = 
    ScalaEnumInfo(
      StringByteEngine.read(bbuf),
      ArrayStringByteEngine.read(bbuf)
      )

case class ScalaEnumInfo protected[scala_reflection](
  name: String,
  values: Array[String]
) extends EnumInfo: 
  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  private lazy val ordinalMethod = infoClass.getMethod("ordinal")
  private lazy val valuesMethod  = infoClass.getMethod("values")
  private lazy val valueOfMethod = infoClass.getMethod("valueOf", classOf[String])

  def ordinal(s: String): Int = ordinalMethod.invoke(valueOfMethod.invoke(null, s)).asInstanceOf[Int]
  def valueOf(s: String): Any = valueOfMethod.invoke(null,s)
  def valueOf(i: Int): Any = valuesMethod.invoke(null).asInstanceOf[Array[Object]].find(e => ordinalMethod.invoke(e).asInstanceOf[Int] == i).get

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( ENUM_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayStringByteEngine.write(bbuf, values)

//---------------------------------------------------------

object ScalaEnumerationInfo:
  def fromBytes( bbuf: ByteBuffer ): ScalaEnumerationInfo = 
    ScalaEnumerationInfo(
      StringByteEngine.read(bbuf),
      ArrayStringByteEngine.read(bbuf)
      )

case class ScalaEnumerationInfo protected[scala_reflection](
  name: String,
  values: Array[String]
) extends EnumInfo:
  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  private lazy val withNameMethod = infoClass.getMethod("withName", classOf[String])
  private lazy val applyMethod = infoClass.getMethod("apply", classOf[Int])

  def ordinal(s: String): Int = valueOf(s).asInstanceOf[Enumeration#Value].id
  def valueOf(s: String): Any = withNameMethod.invoke(null,s)
  def valueOf(i: Int): Any = applyMethod.invoke(null,i.asInstanceOf[Object])

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( ENUMERATION_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayStringByteEngine.write(bbuf, values)

//---------------------------------------------------------

object JavaEnumInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaEnumInfo = 
    JavaEnumInfo(
      StringByteEngine.read(bbuf)
      )

case class JavaEnumInfo protected[scala_reflection](
  name: String,
) extends RType: 
  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName +s"(${infoClass.getName})\n"

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_ENUM_INFO )
    StringByteEngine.write(bbuf, name)
  