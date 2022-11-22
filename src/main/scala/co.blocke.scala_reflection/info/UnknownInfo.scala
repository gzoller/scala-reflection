package co.blocke.scala_reflection
package info

import java.nio.ByteBuffer
import impl.*

/** This is for all the classes we don't inspect.  These may be "invalid" or just not reflectable.
  * Rather than toss our exception cookies, we just return UnknownInfo and let the caller decide
  * how serious this is.  In the case of ScalaJack, it may be completely fine, for example UUID.
  * We can make a ScalaJack TypeAdapter for UUID without needing to inspect the type.  For some
  * other application an UnknownInfo might be a serious problem.
  */
object UnknownInfo:
  def fromBytes( bbuf: ByteBuffer ): UnknownInfo = UnknownInfo(StringByteEngine.read(bbuf))


case class UnknownInfo(name: String) extends RType:

  val fullName = name

  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
    {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put(UNKNOWN_INFO)
    StringByteEngine.write(bbuf, name)

  def jsSerialize(sb: StringBuffer): Unit =
    sb.append(s"""{"kind":"unknown","name":"$name","fullName":"$fullName"}""")
