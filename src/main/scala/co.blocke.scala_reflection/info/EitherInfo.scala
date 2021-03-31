package co.blocke.scala_reflection
package info

import impl.*
import java.nio.ByteBuffer

object EitherInfo:
  def fromBytes( bbuf: ByteBuffer ): EitherInfo = 
    EitherInfo(
      StringByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf),
      RTypeByteEngine.read(bbuf)
      )

case class EitherInfo protected[scala_reflection](
  name: String,
  _leftType: RType,
  _rightType: RType
) extends RType with LeftRightRType: 

  val fullName: String = name + "[" + _leftType.fullName + "," + _rightType.fullName + "]"

  lazy val infoClass: Class[_] = Class.forName(name)

  lazy val leftType: RType = _leftType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val rightType: RType = _rightType match {
    case e: SelfRefRType => e.resolve
    case e => e
  }

  def _copy( left: RType, right: RType ) = EitherInfo(name, left, right)

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( EITHER_INFO )
    StringByteEngine.write(bbuf, name)
    RTypeByteEngine.write(bbuf, _leftType)
    RTypeByteEngine.write(bbuf, _rightType)


