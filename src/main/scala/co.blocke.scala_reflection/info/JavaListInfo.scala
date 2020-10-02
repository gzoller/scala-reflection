package co.blocke.scala_reflection
package info

import impl._
import java.nio.ByteBuffer


/** Java List dirivative */
object JavaListInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaListInfo =
    JavaListInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaListInfo protected[scala_reflection](
  name: String,
  _elementType: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaListInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaListInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
     
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_LIST_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)
