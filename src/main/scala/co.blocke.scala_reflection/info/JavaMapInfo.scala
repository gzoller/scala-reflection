package co.blocke.scala_reflection
package info

import impl.*
import scala.quoted.Quotes
import java.nio.ByteBuffer


/** Java Map dirivative */
object JavaMapInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaMapInfo =
    JavaMapInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class JavaMapInfo protected[scala_reflection](
  name: String,
  _elementType: RType,
  _elementType2: RType
) extends RType with CollectionRType:

  val fullName = name + "[" + _elementType.fullName + "," + _elementType2.fullName + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  
  lazy val elementType: RType = _elementType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val elementType2: RType = _elementType2 match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
    import quotes.reflect.{_, given}
    implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx 
    dotty.tools.dotc.core.Types.AppliedType(
      TypeRepr.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(elementType.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type], elementType2.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[quotes.reflect.AppliedType]

  override def select(i: Int): RType = 
    i match {
      case 0 => elementType
      case 1 => elementType2
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
    } 

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    val stage1 = _elementType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaMapInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]), _elementType2)
      case art: AppliedRType if art.isAppliedType => 
        JavaMapInfo(name, art.resolveTypeParams(paramMap), _elementType2)
      case _ => this
    }
    _elementType2 match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
        JavaMapInfo(name, stage1._elementType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => 
        JavaMapInfo(name, stage1._elementType, art.resolveTypeParams(paramMap))
      case _ => stage1
    }
  
  override def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name):\n"
    + elementType.show(newTab,name :: seenBefore)
    + elementType2.show(newTab,name :: seenBefore)
      
  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_MAP_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _elementType)
    RTypeByteEngine.write(bbuf, _elementType2)