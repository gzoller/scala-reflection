package co.blocke.scala_reflection
package info

import scala.quoted.Quotes
import impl.*
import java.nio.ByteBuffer


object TraitInfo:
  def fromBytes( bbuf: ByteBuffer ): TraitInfo = 
    TraitInfo(
      StringByteEngine.read(bbuf),
      ArrayFieldInfoByteEngine.read(bbuf),
      ArrayRTypeByteEngine.read(bbuf),
      ArrayStringByteEngine.read(bbuf).asInstanceOf[Array[TypeSymbol]]
      )

case class TraitInfo protected[scala_reflection](
    name: String, 
    fields: Array[FieldInfo],
    actualParameterTypes: Array[RType] = Array.empty[RType],
    paramSymbols: Array[TypeSymbol] = Array.empty[TypeSymbol],
  ) extends RType with AppliedRType: 

  val fullName: String = 
    if actualParameterTypes.nonEmpty then
      name + actualParameterTypes.map(_.fullName).toList.mkString("[",",","]")
    else
      name
  lazy val infoClass: Class[_] = Class.forName(name)
 
  override def isAppliedType: Boolean = paramSymbols.nonEmpty

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType =
    TraitInfo(
      name, 
      fields.map( _.asInstanceOf[ScalaFieldInfo].resolveTypeParams(paramMap) ),
      actualParameterTypes.map( _ match {
          case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => paramMap(ts.name.asInstanceOf[TypeSymbol])
          case art: AppliedRType if art.isAppliedType => art.resolveTypeParams(paramMap)
          case t => t
        }),
      paramSymbols
      )

  override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
    import quotes.reflect.{_, given}
    if actualParameterTypes.nonEmpty then
      val args = actualParameterTypes.map(_.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type]).toList
      implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx 
      dotty.tools.dotc.core.Types.AppliedType(
        TypeRepr.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
        args
        ).asInstanceOf[quotes.reflect.AppliedType]
    else
      quotes.reflect.TypeRepr.typeConstructorOf(infoClass)
      
  def select(i: Int): RType = 
    if i >= 0 && i <= actualParameterTypes.size-1 then
      actualParameterTypes(i)
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")   

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}

    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      val params = 
        if actualParameterTypes.isEmpty then 
          "" 
        else 
          val syms = actualParameterTypes.zip(paramSymbols)
          " actualParamTypes: [\n"+syms.map{ (ap:RType, s:TypeSymbol) => tabs(tab+1) + s.toString+": "+ap.show(tab+2,name :: seenBefore, true) }.mkString + tabs(tab) + "]"
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name)$params with fields:\n"
      + { fields.toList.map(f => tabs(tab+1)+f.name+{if f.originalSymbol.isDefined then "["+f.originalSymbol.get.toString+"]" else ""}+": "+f.fieldType.show(tab+1, Nil, true)).mkString("") }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( TRAIT_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayFieldInfoByteEngine.write(bbuf, fields)
    ArrayRTypeByteEngine.write(bbuf, actualParameterTypes)
    ArrayStringByteEngine.write(bbuf, paramSymbols.asInstanceOf[Array[String]])

//------------------------------------------------------------

object SealedTraitInfo:
  def fromBytes( bbuf: ByteBuffer ): SealedTraitInfo = 
    SealedTraitInfo(
      StringByteEngine.read(bbuf),
      ArrayRTypeByteEngine.read(bbuf)
      )

case class SealedTraitInfo protected[scala_reflection](
    name: String, 
    children: Array[RType]
  ) extends RType:

  val fullName: String = name + children.map(_.fullName).toList.mkString("[",",","]")
  lazy val infoClass: Class[_] = Class.forName(name)

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    if seenBefore.contains(name) then
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
    else
      {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
      + s"($name)"
      + {if children.isEmpty then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1,name :: seenBefore)).mkString}

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( SEALED_TRAIT_INFO )
    StringByteEngine.write(bbuf, name)
    ArrayRTypeByteEngine.write(bbuf, children)
    
  