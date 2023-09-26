package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes


trait OptionRType[R] extends RType[R] with AppliedRType:
  lazy val optionParamType: RType[_]

  def select(i: Int): RType[_] = 
    if i == 0 then
      optionParamType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
      

//-------------------


case class ScalaOptionRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _optionParamType: RType[_]
) extends OptionRType[R]:

  val typedName: TypedName = name + "[" + _optionParamType.typedName + "]"
  def selectLimit: Int = 1

  lazy val clazz: Class[_] = Class.forName(name)
  lazy val optionParamType: RType[_] = _optionParamType match {
    // case e: SelfRefRType => e.resolve
    case e => e
  }

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val optType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[_optionParamType.T] = _optionParamType.toType(quotes)
    val optTypeRepr = TypeRepr.of[R](using optType)
    val paramTypeRepr = TypeRepr.of[_optionParamType.T](using paramType)
    AppliedType(optTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

//-------------------

/*
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
   
  def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
    val newTab = {if suppressIndent then tab else tab+1}
    {if(!suppressIndent) tabs(tab) else ""} + "Optional of " + optionParamType.show(newTab,name :: seenBefore,true)

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

  def jsSerialize(sb: StringBuffer): Unit =
    sb.append(s"""{"kind":"Java option","name":"$name","fullName":"$fullName","_optionParamType":""")
    _optionParamType.jsSerialize(sb)
    sb.append("}")
*/