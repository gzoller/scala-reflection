package co.blocke.scala_reflection
package rtypes

import java.lang.reflect.Method
import java.nio.ByteBuffer


trait FieldInfo[R] extends Serializable:
  type T = R 
  val index:                     Int
  val name:                      String
  val fieldType:                 RType[R]
  // val originalSymbol:            Option[TypeSymbol]
  val annotations:               Map[String,Map[String,String]]
  // lazy val defaultValue:         Option[Object]

  def reIndex(i: Int): FieldInfo[R]
  def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): FieldInfo[R]
  // val fieldType = UnknownRType("java.lang.String")

//------------------------------------------------------------

case class ScalaFieldInfo[R](
  index:                    Int,
  name:                     String,
  fieldType:                RType[R],
  annotations:              Map[String,Map[String,String]],
  // defaultValueAccessorName: Option[(String,String)], // (companion class name, method) 
  // originalSymbol:           Option[TypeSymbol],
  // isNonValConstructorField: Boolean = false,
) extends FieldInfo[R]:

  val defaultValueAccessorName: Option[(String,String)] = None

  def reIndex(i: Int): FieldInfo[R] = this.copy(index = i)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): FieldInfo[R] = 
    fieldType match {
    //   case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => this.copy(fieldType = paramMap(ts.name.asInstanceOf[TypeSymbol]))
    //   case art: AppliedRType if art.isAppliedType => this.copy(fieldType = art.resolveTypeParams(paramMap))
      case _ => this
    }

  lazy val defaultValue: Option[Object] = defaultValueAccessorName.map{ (companionClass, accessor) =>
    val companion = Class.forName(companionClass)
    val cons = companion.getDeclaredConstructors()
    cons(0).setAccessible(true)
    companion.getMethod(accessor).invoke( cons(0).newInstance() )
  }


//------------------------------------------------------------
/*
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
*/
