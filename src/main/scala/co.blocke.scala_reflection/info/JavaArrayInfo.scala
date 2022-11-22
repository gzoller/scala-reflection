package co.blocke.scala_reflection
package info

import impl.*
import java.nio.ByteBuffer


/** Java Array */
object JavaArrayInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaArrayInfo =
    JavaArrayInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaArrayInfo protected[scala_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:
 
  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaArrayInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaArrayInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if suppressIndent then tab else tab+1}
    {if(!suppressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,name :: seenBefore,true)
     
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_ARRAY_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)

  def jsSerialize(sb: StringBuffer): Unit =
    sb.append(s"""{"kind":"Java array","name":"$name","fullName":"$fullName","_elementType":""")
    _elementType.jsSerialize(sb)
    sb.append("}")
