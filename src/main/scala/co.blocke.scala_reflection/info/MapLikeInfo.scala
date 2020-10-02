package co.blocke.scala_reflection
package info

import impl._
import scala.tasty.Reflection
import java.nio.ByteBuffer


/** Arity 2 Collections, Map flavors, basiclly */
object MapLikeInfo:
  def fromBytes( bbuf: ByteBuffer ): MapLikeInfo =
    MapLikeInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class MapLikeInfo protected[scala_reflection](
  name: String,
  _elementType: RType,
  _elementType2: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "," + _elementType2.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    implicit val stuff = reflect.rootContext.asInstanceOf[dotty.tools.dotc.core.Contexts.Context] 
    dotty.tools.dotc.core.Types.AppliedType(
      Type.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(elementType.toType(reflect).asInstanceOf[dotty.tools.dotc.core.Types.Type], elementType2.toType(reflect).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[reflect.AppliedType]

  override def select(i: Int): RType = 
    i match {
      case 0 => elementType
      case 1 => elementType2
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
    }     
    
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    val stage1 = _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        MapLikeInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]), _elementType2)
      case art: AppliedRType if art.isAppliedType => 
        MapLikeInfo(name, art.resolveTypeParams(paramMap), _elementType2)
      case _ => this
    }
    _elementType2 match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        MapLikeInfo(name, stage1._elementType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        MapLikeInfo(name, stage1._elementType, art.resolveTypeParams(paramMap))
      case _ => stage1
    }

  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val elementType2: RType = _elementType2 match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name):\n"
    + elementType.show(newTab)
    + elementType2.show(newTab)

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( MAPLIKE_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)
    RTypeByteEngine.write(bbuf, _elementType2)
