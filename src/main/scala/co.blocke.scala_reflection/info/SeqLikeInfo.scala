package co.blocke.scala_reflection
package info

import impl._
import java.nio.ByteBuffer


/** Arity 1 Collections, e.g. List, Set, Seq */
object SeqLikeInfo:
  def fromBytes( bbuf: ByteBuffer ): SeqLikeInfo =
    SeqLikeInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class SeqLikeInfo protected[scala_reflection](
  name: String,
  _elementType: RType,
) extends RType with CollectionRType:

  val fullName: String = name + "[" + _elementType.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
      
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType =
    _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        SeqLikeInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        SeqLikeInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( SEQLIKE_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)

