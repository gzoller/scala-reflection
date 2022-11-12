package co.blocke.scala_reflection
package info

import java.lang.reflect.*
import java.util.Optional
import impl.*
import scala.quoted.Quotes
import java.nio.ByteBuffer


trait OptionInfo extends RType with AppliedRType:
  lazy val optionParamType: RType

  def select(i: Int): RType = 
    if i == 0 then
      optionParamType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
      


object ScalaOptionInfo:
  def fromBytes( bbuf: ByteBuffer ): ScalaOptionInfo =
    ScalaOptionInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

//-------------------

case class ScalaOptionInfo protected[scala_reflection](
  name: String,
  _optionParamType: RType
) extends OptionInfo:

  val fullName: String = name + "[" + _optionParamType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _optionParamType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => ScalaOptionInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => ScalaOptionInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }

  override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
    import quotes.reflect.{_, given}
    implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx 
    dotty.tools.dotc.core.Types.AppliedType(
      TypeRepr.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(optionParamType.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[quotes.reflect.AppliedType]
    
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Option of " + optionParamType.show(newTab,name :: seenBefore,true)

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( OPTION_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _optionParamType)


object JavaOptionalInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaOptionalInfo =
    JavaOptionalInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

//-------------------
      
case class JavaOptionalInfo protected[scala_reflection](
  name: String,
  _optionParamType: RType
) extends OptionInfo:

  val fullName: String = name + "[" + _optionParamType.fullName  + "]"
  lazy val infoClass: Class[_] = Class.forName(name)
  lazy val optionParamType: RType = _optionParamType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
    import quotes.reflect.{_, given}
    implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx 
    dotty.tools.dotc.core.Types.AppliedType(
      TypeRepr.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(optionParamType.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[quotes.reflect.AppliedType]
   
  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + "Optional of " + optionParamType.show(newTab,name :: seenBefore,true)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    _optionParamType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => JavaOptionalInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => JavaOptionalInfo(name, art.resolveTypeParams(paramMap))
      case _ => this
    }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( OPTIONAL_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _optionParamType)