package co.blocke.scala_reflection
package info

import java.nio.ByteBuffer
import impl.*

/** RType for unassigned type symbol, e.g. Foo[T]
 */

object TypeSymbolInfo:
  def fromBytes( bbuf: ByteBuffer ): TypeSymbolInfo = TypeSymbolInfo(StringByteEngine.read(bbuf))

case class TypeSymbolInfo(name: String) extends RType:
  val fullName = name
  lazy val infoClass = impl.Clazzes.ObjectClazz
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    {if(!supressIndent) tabs(tab) else ""} + name + "\n"
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put(TYPE_SYMBOL_INFO)
    StringByteEngine.write(bbuf, name)
