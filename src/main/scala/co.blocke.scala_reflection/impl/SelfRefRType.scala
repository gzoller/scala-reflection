package co.blocke.scala_reflection
package impl

import java.nio.ByteBuffer

/** Placeholder RType to be lazy-resolved, used for self-referencing types.  This is needed because without it, reflecting on
 *  a self-referencing type will enter an endless loop until the stack explodes.  This RType is immediately inserted into the
 *  type cache so that when the self-reference comes there's something in the cache to find.
 *  When one of these is encountered in the wild, just re-Reflect on the infoClass and you'll get the non-SelfRef (i.e. normal) RType
 */
object SelfRefRType:
  def fromBytes( bbuf: ByteBuffer ): SelfRefRType = SelfRefRType(StringByteEngine.read(bbuf))

case class SelfRefRType(name: String) extends RType:
  val fullName: String = name
  lazy val infoClass: Class[_] = Class.forName(name)
  def resolve: RType = RType.of(infoClass)
  def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = s"SelfRefRType of $name" 
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put(SELFREF)
    StringByteEngine.write(bbuf, name)