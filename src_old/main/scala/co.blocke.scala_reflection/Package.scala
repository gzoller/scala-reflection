package co.blocke.scala_reflection

import impl.*

import scala.annotation.tailrec
// import scala.runtime.Statics.releaseFence

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 

/** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val UNION_CLASS = "Union"

/** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val INTERSECTION_CLASS = "Intersection"

class ReflectException(msg: String) extends Exception(msg)

val ENUM_CLASSNAME = "scala.Enumeration.Value"

val typesymregx = """.*\.\_\$(.+)$""".r

val S3ANNO = "co.blocke.scala_reflection.S3Reflection"

val NONE = info.TypeSymbolInfo("<none>")
    
def mangleArrayClassName(tpe: RType): String =
  val mangled = tpe match {
    case _: info.TypeSymbolInfo => "Ljava.lang.Object;"
    case c: info.ArrayInfo => mangleArrayClassName(c.elementType)
    case c: info.JavaArrayInfo => mangleArrayClassName(c.elementType)
    case PrimitiveType.Scala_Boolean => "Z"
    case PrimitiveType.Scala_Byte => "B"
    case PrimitiveType.Scala_Char => "C"
    case PrimitiveType.Scala_Double => "D"
    case PrimitiveType.Scala_Float => "F"
    case PrimitiveType.Scala_Int => "I"
    case PrimitiveType.Scala_Long => "J"
    case PrimitiveType.Scala_Short => "S"
    case PrimitiveType.Scala_Any => "Ljava.lang.Object;"
    case c => "L" + c.name + ";"
  }
  "[" + mangled


extension [A,B](xs: List[A]) {
  def findMap( p: (A) => Option[B] ): Option[B] = 
    var these: List[A] = xs
    while (these.nonEmpty) {
      val pRet = p(these.head)
      if pRet.isDefined then return pRet
      these = these.tail
    }
    None

  def filterMap(p: A => Option[B]): List[B] =
    @tailrec
    def doit(l: List[A], acc: List[B]): List[B] = {
      if (l.isEmpty)
        acc
      else {
        val retVal = p(l.head)
        val newAcc = if retVal.isDefined then
            acc :+ retVal.get
          else 
            acc
        doit(l.tail, newAcc)
      }
    }
    val result = doit(xs, Nil)
    // releaseFence()  <--- Not sure why this is needed, or what it does!
    result
}


val BUFFER_MAX = 65536 // max number of bytes for serialized RType tree

// Serialized Type Codes
val SCALA_BOOLEAN        : Byte = 1
val SCALA_BYTE           : Byte = 2
val SCALA_CHAR           : Byte = 3
val SCALA_DOUBLE         : Byte = 4
val SCALA_FLOAT          : Byte = 5
val SCALA_INT            : Byte = 6
val SCALA_LONG           : Byte = 7 
val SCALA_SHORT          : Byte = 8
val SCALA_STRING         : Byte = 9
val SCALA_ANY            : Byte = 10
val JAVA_BOOLEAN         : Byte = 11
val JAVA_BYTE            : Byte = 12
val JAVA_CHAR            : Byte = 13
val JAVA_DOUBLE          : Byte = 14
val JAVA_FLOAT           : Byte = 15
val JAVA_INT             : Byte = 16
val JAVA_LONG            : Byte = 17
val JAVA_SHORT           : Byte = 18
val JAVA_OBJECT          : Byte = 19
val JAVA_NUMBER          : Byte = 20
val SELFREF              : Byte = 21
val ALIAS_INFO           : Byte = 22
val SCALA_CASE_CLASS_INFO: Byte = 23
val SCALA_CLASS_INFO     : Byte = 24
val JAVA_CLASS_INFO      : Byte = 25
val SEQLIKE_INFO         : Byte = 26
val MAPLIKE_INFO         : Byte = 27
val ARRAY_INFO           : Byte = 28
val JAVA_SET_INFO        : Byte = 29
val JAVA_LIST_INFO       : Byte = 30
val JAVA_ARRAY_INFO      : Byte = 31
val JAVA_QUEUE_INFO      : Byte = 32
val JAVA_STACK_INFO      : Byte = 33
val JAVA_MAP_INFO        : Byte = 34
val EITHER_INFO          : Byte = 35
val ENUM_INFO            : Byte = 36
val ENUMERATION_INFO     : Byte = 37
val JAVA_ENUM_INFO       : Byte = 38
val INTERSECTION_INFO    : Byte = 39
val OBJECT_INFO          : Byte = 40
val OPTION_INFO          : Byte = 41
val OPTIONAL_INFO        : Byte = 42
val SCALA2_INFO          : Byte = 43
val TRAIT_INFO           : Byte = 44
val TYPE_SYMBOL_INFO     : Byte = 45
val SEALED_TRAIT_INFO    : Byte = 46
val TRY_INFO             : Byte = 47
val TUPLE_INFO           : Byte = 48
val TYPE_MEMBER_INFO     : Byte = 49
val UNION_INFO           : Byte = 50
val UNKNOWN_INFO         : Byte = 51
val SCALA_FIELD_INFO     : Byte = 52
val JAVA_FIELD_INFO      : Byte = 53
val JAVA_CLASS_INFO_PROXY: Byte = 54
