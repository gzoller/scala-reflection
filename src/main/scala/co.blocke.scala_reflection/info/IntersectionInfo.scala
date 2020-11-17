package co.blocke.scala_reflection
package info

import scala.tasty.Reflection
import java.nio.ByteBuffer
import impl._


object IntersectionInfo:
  def fromBytes( bbuf: ByteBuffer ): IntersectionInfo = 
    IntersectionInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class IntersectionInfo protected[scala_reflection](
  name: String,
  _leftType: RType,
  _rightType: RType
  ) extends RType with LeftRightRType:

    val fullName: String = name + "[" + _leftType.fullName + "," + _rightType.fullName + "]"

    lazy val infoClass: Class[_] = impl.Clazzes.AnyClazz

    lazy val leftType: RType = _leftType match {
      case e: SelfRefRType => e.resolve
      case e => e
    }
    lazy val rightType: RType = _rightType match {
      case e: SelfRefRType => e.resolve
      case e => e
    } 

    override def toType(reflect: Reflection): reflect.TypeRepr = 
      import reflect.{_, given}
      AndType(leftType.toType(reflect), rightType.toType(reflect))  

    def _copy( left: RType, right: RType ) = this.copy(_leftType = left, _rightType = right)
    
    def toBytes( bbuf: ByteBuffer ): Unit = 
      bbuf.put( INTERSECTION_INFO )
      StringByteEngine.write(bbuf, name)
      RTypeByteEngine.write(bbuf, _leftType)
      RTypeByteEngine.write(bbuf, _rightType)