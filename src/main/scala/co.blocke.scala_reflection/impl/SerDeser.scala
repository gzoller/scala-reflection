package co.blocke.scala_reflection
package impl

import java.io._
import java.nio._
import scala.reflect.ClassTag
import info.{FieldInfo, ScalaFieldInfo, JavaFieldInfo}

trait BytesEngine[T]:
  def write( bbuf: ByteBuffer, t: T ): Unit
  def read( bbuf: ByteBuffer ): T


object RTypeByteEngine extends BytesEngine[RType]:
  def write( bbuf: ByteBuffer, t: RType ): Unit = t.toBytes(bbuf)
  def read( bbuf: ByteBuffer ): RType = RType.fromBytes(bbuf)


object FieldInfoByteEngine extends BytesEngine[FieldInfo]:
  def write( bbuf: ByteBuffer, t: FieldInfo ): Unit = 
    t match {
      case s: ScalaFieldInfo => s.toBytes(bbuf)
      case j: JavaFieldInfo => j.toBytes(bbuf)
    }
  def read( bbuf: ByteBuffer ): FieldInfo = 
    bbuf.get() match {
      case SCALA_FIELD_INFO => ScalaFieldInfo.fromBytes(bbuf)
      case JAVA_FIELD_INFO => JavaFieldInfo.fromBytes(bbuf)
    }


// *** CUSTOM ***
object MapStringByteEngine extends BytesEngine[Map[String,Map[String,String]]]:
  def write( bbuf: ByteBuffer, t: Map[String,Map[String,String]] ): Unit =
    bbuf.putInt( t.size )
    t.foreach{ (k,v) => 
      StringByteEngine.write(bbuf,k) 
      bbuf.putInt( v.size )
      v.foreach{ (k2, v2) => 
        StringByteEngine.write(bbuf,k2) 
        StringByteEngine.write(bbuf,v2) 
      }
    }

  def read( bbuf: ByteBuffer ): Map[String,Map[String,String]] =
    val len = bbuf.getInt()
    (0 to len-1).map{ _ => 
      val k = StringByteEngine.read(bbuf)
      val vlen = bbuf.getInt()
      val v: Map[String,String] = (0 to vlen-1).map {_ =>
        val k2 = StringByteEngine.read(bbuf)
        val v2 = StringByteEngine.read(bbuf)
        (k2,v2)
      }.toMap
      (k,v)
    }.toMap


// *** CUSTOM ***
object MapStringListByteEngine extends BytesEngine[Map[String,Map[String,List[Int]]]]:
  def write( bbuf: ByteBuffer, t: Map[String,Map[String,List[Int]]] ): Unit =
    bbuf.putInt( t.size )
    t.foreach{ (k,v) => 
      StringByteEngine.write(bbuf,k)
      bbuf.putInt( v.size )
      v.foreach{ (k2,v2) =>
        StringByteEngine.write(bbuf,k2)
        bbuf.putInt(v2.length)
        v2.foreach( one => bbuf.putInt(one) )
      }
    }

  def read( bbuf: ByteBuffer ): Map[String,Map[String,List[Int]]] =
    val len = bbuf.getInt()
    (0 to len-1).map{ _ => 
      val k = StringByteEngine.read(bbuf)
      val len2 = bbuf.getInt()
      val v = (0 to len2-1).map{ _ =>
        val k2 = StringByteEngine.read(bbuf)
        val arrLen = bbuf.getInt()
        val v2 = (0 to arrLen-1).map(_ => bbuf.getInt()).toList
        (k2,v2)
      }.toMap
      (k,v)
    }.toMap

    
// *** CUSTOM ***
object MapStringRTypeByteEngine extends BytesEngine[Map[String,RType]]:
  def write( bbuf: ByteBuffer, t: Map[String,RType] ): Unit =
    bbuf.putInt( t.size )
    t.foreach{ (k,v) => 
      StringByteEngine.write(bbuf,k) 
      RTypeByteEngine.write(bbuf,v) 
    }

  def read( bbuf: ByteBuffer ): Map[String,RType] =
    val len = bbuf.getInt()
    (0 to len-1).map{ _ => 
      val k = StringByteEngine.read(bbuf)
      val v = RTypeByteEngine.read(bbuf)
      (k,v)
    }.toMap


// *** CUSTOM ***
object ArrayStringByteEngine extends BytesEngine[Array[String]]:
  def write( bbuf: ByteBuffer, t: Array[String] ): Unit =
    bbuf.putInt( t.length )
    t.foreach( one => StringByteEngine.write(bbuf, one) )

  def read( bbuf: ByteBuffer ): Array[String] =
    val len = bbuf.getInt()
    (0 to len-1).map(_ => StringByteEngine.read(bbuf)).toArray


// *** CUSTOM ***
object ArrayRTypeByteEngine extends BytesEngine[Array[RType]]:
  def write( bbuf: ByteBuffer, t: Array[RType] ): Unit =
    bbuf.putInt( t.length )
    t.foreach( one => RTypeByteEngine.write(bbuf, one) )

  def read( bbuf: ByteBuffer ): Array[RType] =
    val len = bbuf.getInt()
    (0 to len-1).map(_ => RTypeByteEngine.read(bbuf)).toArray


// *** CUSTOM ***
object ArrayFieldInfoByteEngine extends BytesEngine[Array[FieldInfo]]:
  def write( bbuf: ByteBuffer, t: Array[FieldInfo] ): Unit =
    bbuf.putInt( t.length )
    t.foreach( one => FieldInfoByteEngine.write(bbuf, one) )

  def read( bbuf: ByteBuffer ): Array[FieldInfo] =
    val len = bbuf.getInt()
    (0 to len-1).map(_ => FieldInfoByteEngine.read(bbuf)).toArray


// *** CUSTOM ***
object OptionArrayStringByteEngine extends BytesEngine[Option[Array[String]]]:
  def write( bbuf: ByteBuffer, t: Option[Array[String]] ): Unit =
    if t.isDefined then 
      bbuf.put(1.toByte)
      bbuf.putInt( t.get.length )
      t.get.foreach( one => StringByteEngine.write(bbuf, one) )
    else
      bbuf.put(0.toByte)

  def read( bbuf: ByteBuffer ): Option[Array[String]] =
    bbuf.get() match {
      case 0 => None
      case 1 => Some{
          val len = bbuf.getInt()
          (0 to len-1).map(_ => StringByteEngine.read(bbuf)).toArray    
        }
    }


// *** CUSTOM ***
object OptionRTypeByteEngine extends BytesEngine[Option[RType]]:
  def write( bbuf: ByteBuffer, t: Option[RType] ): Unit =
    if t.isDefined then 
      bbuf.put(1.toByte)
      RTypeByteEngine.write(bbuf, t.get)
    else
      bbuf.put(0.toByte)

  def read( bbuf: ByteBuffer ): Option[RType] =
    bbuf.get() match {
      case 0 => None
      case 1 => Some(RTypeByteEngine.read(bbuf))
    }


// *** CUSTOM ***
object OptionStringByteEngine extends BytesEngine[Option[String]]:
  def write( bbuf: ByteBuffer, t: Option[String] ): Unit =
    if t.isDefined then 
      bbuf.put(1.toByte)
      StringByteEngine.write(bbuf, t.get)
    else
      bbuf.put(0.toByte)

  def read( bbuf: ByteBuffer ): Option[String] =
    bbuf.get() match {
      case 0 => None
      case 1 => Some(StringByteEngine.read(bbuf))
    }



object StringByteEngine extends BytesEngine[String]:
  def write( bbuf: ByteBuffer, t: String ): Unit =
    bbuf.putInt(t.length)
    bbuf.put(t.getBytes)

  def read( bbuf: ByteBuffer ): String =
    val len = bbuf.getInt()
    val byteArray = new Array[Byte](len)
    bbuf.get( byteArray, 0, len )
    new String(byteArray, 0, len)


object BooleanByteEngine extends BytesEngine[Boolean]:
  def write( bbuf: ByteBuffer, t: Boolean ): Unit =
    if t then
      bbuf.put(1.toByte)
    else
      bbuf.put(0.toByte)

  def read( bbuf: ByteBuffer ): Boolean =
    bbuf.get() match {
      case 0 => false
      case 1 => true
    }

/*
case class ArrayByteEngine[T:ClassTag](elementEngine: BytesEngine[T]) extends BytesEngine[Array[T]]:
  def write( bbuf: ByteBuffer, t: Array[T] ): Unit =
    bbuf.putInt( t.length )
    t.foreach( one => elementEngine.write(bbuf, one) )

  def read( bbuf: ByteBuffer ): Array[T] =
    val len = bbuf.getInt()
    (0 to len-1).map(_ => elementEngine.read(bbuf)).toArray


case class MapByteEngine[K:ClassTag, V:ClassTag](keyEngine: BytesEngine[K], valueEngine: BytesEngine[V]) extends BytesEngine[Map[K,V]]:
  def write( bbuf: ByteBuffer, t: Map[K,V] ): Unit =
    bbuf.putInt( t.size )
    t.foreach{ (k,v) => 
      keyEngine.write(bbuf,k) 
      valueEngine.write(bbuf,v) 
    }

  def read( bbuf: ByteBuffer ): Map[K,V] =
    val len = bbuf.getInt()
    (0 to len-1).map{ _ => 
      val k = keyEngine.read(bbuf)
      val v = valueEngine.read(bbuf)
      (k,v)
    }.toMap


case class OptionByteEngine[T](elementEngine: BytesEngine[T]) extends BytesEngine[Option[T]]:
  def write( bbuf: ByteBuffer, t: Option[T] ): Unit =
    if t.isDefined then 
      bbuf.put(1.toByte)
      elementEngine.write(bbuf, t.get)
    else
      bbuf.put(0.toByte)

  def read( bbuf: ByteBuffer ): Option[T] =
    bbuf.get() match {
      case 0 => None
      case 1 => Some( elementEngine.read(bbuf) )
    }
    */
  

// This one's gonna be slow, but how else are we gonna serialize a Method?
object ObjectByteEngine extends BytesEngine[Object]:
  def write( bbuf: ByteBuffer, t: Object ): Unit =
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream( baos )
    oos.writeObject( t )
    oos.close()
    val array = baos.toByteArray()
    bbuf.putInt( array.length )
    bbuf.put(array)

  def read( bbuf: ByteBuffer ): Object =
    val len = bbuf.getInt()
    val arrayBuf = new Array[Byte](len)
    bbuf.get(arrayBuf, bbuf.position, len)
    val ois = new ObjectInputStream( new ByteArrayInputStream( arrayBuf ) )
    val o   = ois.readObject()
    ois.close()
    o
