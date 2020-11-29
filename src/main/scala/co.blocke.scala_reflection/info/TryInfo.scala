package co.blocke.scala_reflection
package info

import scala.util.Try
import impl._
import scala.quoted.Quotes
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

  override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
    import quotes.reflect.{_, given}
    implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
    dotty.tools.dotc.core.Types.AppliedType(
      TypeRepr.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(tryType.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[quotes.reflect.AppliedType]

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