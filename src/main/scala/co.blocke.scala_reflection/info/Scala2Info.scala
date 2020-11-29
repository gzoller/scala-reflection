package co.blocke.scala_reflection
package info

import java.nio.ByteBuffer
import impl._

/** RType for a Scala 2 class (no Tasty info)
 */
object Scala2Info:
  def fromBytes( bbuf: ByteBuffer ): Scala2Info = Scala2Info(StringByteEngine.read(bbuf))

case class Scala2Info(name: String) extends RType:
  val fullName = name
  lazy val infoClass = Class.forName(name)
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put(SCALA2_INFO)
    StringByteEngine.write(bbuf, name)
