package co.blocke.scala_reflection
package info

import java.lang.reflect.Method
import java.nio.ByteBuffer
import impl.*


trait FieldInfo extends Serializable:
  val index:                     Int
  val name:                      String
  val fieldType:                 RType
  val originalSymbol:            Option[TypeSymbol]
  val annotations:               Map[String,Map[String,String]]
  lazy val defaultValue:         Option[Object]

  def valueOf[T](target: T): Object
  def reIndex(i: Int): FieldInfo

  def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): FieldInfo

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""}
      + s"(${if !modified then index else '_'})" 
      + {if originalSymbol.isDefined then s"[${originalSymbol.get}]" else ""}
      + s" $name: "
      + fieldType.show(newTab,name :: seenBefore,true) 
      + { if annotations.nonEmpty then tabs(newTab) + "annotations: " + annotations.toString + "\n" else "" }

//------------------------------------------------------------

object ScalaFieldInfo:
  def fromBytes( bbuf: ByteBuffer ): ScalaFieldInfo =
    ScalaFieldInfo(
      bbuf.getInt(),
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      MapStringByteEngine.read(bbuf),
      OptionArrayStringByteEngine.read(bbuf).map(found => (found(0),found(1))),
      OptionStringByteEngine.read(bbuf).asInstanceOf[Option[TypeSymbol]],
      BooleanByteEngine.read(bbuf),
      BooleanByteEngine.read(bbuf)
      )

case class ScalaFieldInfo(
  index:                    Int,
  name:                     String,
  fieldType:                RType,
  annotations:              Map[String,Map[String,String]],
  defaultValueAccessorName: Option[(String,String)], // (class, method)  //Option[()=>Object],
  originalSymbol:           Option[TypeSymbol],
  isNonConstructorField:    Boolean = false,
  isNonValConstructorField: Boolean = false,
) extends FieldInfo:

  def valueOf[T](target: T) = target.getClass.getMethod(name).invoke(target)
  def constructorClass: Class[_] = constructorClassFor(fieldType)

  def reIndex(i: Int): FieldInfo = this.copy(index = i)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): FieldInfo = 
    fieldType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => this.copy(fieldType = paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => this.copy(fieldType = art.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val defaultValue: Option[Object] = defaultValueAccessorName.map{ (companionClass, accessor) =>
    val companion = Class.forName(companionClass)
    val cons = companion.getDeclaredConstructors()
    cons(0).setAccessible(true)
    val companionInst = cons(0).newInstance()
    companion.getMethod(accessor).invoke(companionInst)
  }

  private def constructorClassFor(t: RType): Class[_] = 
    t match {
      case _: TypeSymbolInfo => classOf[Object] // Magic: Java constructors set param type to Object if it is a parameterized type
      case info: UnionInfo => classOf[Object]  // Union-typed constructors translate to Object in Java, so...
      case _ if originalSymbol.isDefined => classOf[Object]
      case info: AliasInfo => info.unwrappedType.infoClass
      case info: ScalaCaseClassInfo if info.isValueClass => info.fields.head.fieldType.infoClass
      case info: ScalaEnumerationInfo => Class.forName("scala.Enumeration$Value")
      case info => info.infoClass
    }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( SCALA_FIELD_INFO )
    bbuf.putInt(index)
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, fieldType)
    MapStringByteEngine.write(bbuf, annotations)
    OptionArrayStringByteEngine.write(bbuf, defaultValueAccessorName.map( dvan => Array(dvan._1, dvan._2)))
    OptionStringByteEngine.write(bbuf, originalSymbol.asInstanceOf[Option[String]])
    BooleanByteEngine.write(bbuf, isNonConstructorField)
    BooleanByteEngine.write(bbuf, isNonValConstructorField)

//------------------------------------------------------------

object JavaFieldInfo:
  def fromBytes( bbuf: ByteBuffer ): JavaFieldInfo =
    JavaFieldInfo(
      bbuf.getInt(),
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      MapStringByteEngine.read(bbuf),
      ObjectByteEngine.read(bbuf).asInstanceOf[Method],
      ObjectByteEngine.read(bbuf).asInstanceOf[Method],
      OptionStringByteEngine.read(bbuf).asInstanceOf[Option[TypeSymbol]]
      )

/* This is also used for plain-class getter/setter fields */
case class JavaFieldInfo(
  index:           Int,
  name:            String,
  fieldType:       RType,
  annotations:     Map[String,Map[String,String]],
  valueAccessor:   Method,
  valueSetter:     Method,
  originalSymbol:  Option[TypeSymbol]
) extends FieldInfo:
  lazy val defaultValue = None
  def valueOf[T](target: T): Object = valueAccessor.invoke(target)
  def setValue[T](target: T, theValue: Object) = valueSetter.invoke(target, theValue)
  def reIndex(i: Int): FieldInfo = this.copy(index = i)
  def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): FieldInfo = 
    fieldType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => this.copy(fieldType = paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType => this.copy(fieldType = art.resolveTypeParams(paramMap))
      case _ => this
    }

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_FIELD_INFO )
    bbuf.putInt(index)
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, fieldType)
    MapStringByteEngine.write(bbuf, annotations)
    ObjectByteEngine.write(bbuf, valueAccessor)
    ObjectByteEngine.write(bbuf, valueSetter)
    OptionStringByteEngine.write(bbuf, originalSymbol.asInstanceOf[Option[String]])
