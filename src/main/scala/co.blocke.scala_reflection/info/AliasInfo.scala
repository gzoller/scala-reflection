package co.blocke.scala_reflection
package info

import scala.quoted.Quotes
import java.nio.ByteBuffer
import impl.*

object AliasInfo:
  def fromBytes( bbuf: ByteBuffer ): AliasInfo = 
    AliasInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class AliasInfo protected[scala_reflection] (
    definedType: String,
    unwrappedType: RType // Aliases with a parameterized wrapped type are not currently supported, so ConcreteType here.
  ) extends RType:

    val name: String = definedType.drop(definedType.lastIndexOf('.')+1)
    val fullName = name

    lazy val infoClass = unwrappedType.infoClass
    override def toType(quotes: Quotes): quotes.reflect.TypeRepr = unwrappedType.toType(quotes)

    def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = 
      val newTab = {if suppressIndent then tab else tab+1}
      {if(!suppressIndent) tabs(tab) else ""} + s"alias $name defined as " + unwrappedType.show(newTab,name :: seenBefore,true)

    def toBytes( bbuf: ByteBuffer ): Unit = 
      bbuf.put( ALIAS_INFO )
      StringByteEngine.write(bbuf, definedType)
      RTypeByteEngine.write(bbuf, unwrappedType)

    def jsSerialize(sb: StringBuffer): Unit =
      sb.append(s"""{"kind":"alias","name":"$name","fullName":"$fullName","definedType":$definedType,"unwrappedType":""")
      unwrappedType.jsSerialize(sb)
      sb.append("}")


