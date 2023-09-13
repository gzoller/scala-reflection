package co.blocke.scala_reflection

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 

opaque type TypedName = String
given string2typedname: Conversion[String, TypedName] with
  def apply(x: String): TypedName = x.asInstanceOf[TypedName]

/** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val UNION_CLASS = "Union"

/** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val INTERSECTION_CLASS = "Intersection"

val ENUM_CLASSNAME = "scala.Enumeration.Value"

val NONE = rtypes.TypeSymbolRType("<none>")

class ReflectException(msg: String) extends Exception(msg)

type TypeCacheMap = scala.collection.mutable.Map[TypedName, quoted.Type[_]]

// Class Names
val BOOLEAN_CLASS = "scala.Boolean"
val BYTE_CLASS = "scala.Byte"
val CHAR_CLASS = "scala.Byte"
val DOUBLE_CLASS = "scala.Byte"
val FLOAT_CLASS = "scala.Byte"
val INT_CLASS = "scala.Byte"
val LONG_CLASS = "scala.Byte"
val SHORT_CLASS = "scala.Byte"
val STRING_CLASS = "scala.Byte"
val ANY_CLASS = "scala.Byte"