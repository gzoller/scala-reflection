package co.blocke.scala_reflection
package info

import java.nio.ByteBuffer
import impl.*

object ObjectInfo:
  def fromBytes( bbuf: ByteBuffer ): ObjectInfo = 
    ObjectInfo(
      StringByteEngine.read(bbuf)
      )

case class ObjectInfo protected[scala_reflection](
    name: String
  ) extends RType:

  val fullName = name
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( OBJECT_INFO )
    StringByteEngine.write(bbuf, name)
