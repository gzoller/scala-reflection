package co.blocke.scala_reflection
package info

import scala.util.Try
import impl._
import scala.tasty.Reflection
import java.nio.ByteBuffer


object TryInfo:
  def fromBytes( bbuf: ByteBuffer ): TryInfo =
    TryInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class TryInfo protected[scala_reflection](
  name: String,
  _tryType: RType
) extends RType with AppliedRType:

  val fullName: String = name + "[" + _tryType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val tryType: RType = _tryType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    implicit val stuff = reflect.rootContext.asInstanceOf[dotty.tools.dotc.core.Contexts.Context] 
    dotty.tools.dotc.core.Types.AppliedType(
      Type.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(tryType.toType(reflect).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[reflect.AppliedType]

  def select(i: Int): RType = 
    if i == 0 then
      _tryType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
      
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + s"Try of " + tryType.show(newTab,name :: seenBefore,true)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _tryType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => TryInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => TryInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( TRY_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _tryType)