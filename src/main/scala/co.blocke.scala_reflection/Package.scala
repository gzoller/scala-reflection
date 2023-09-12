package co.blocke.scala_reflection

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 

/** A union type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val UNION_CLASS = "Union"

/** An intersection type is resolved to AnyRef, which isn't helpful.  This is a marker class name to differentiate a union type */
val INTERSECTION_CLASS = "Intersection"

val ENUM_CLASSNAME = "scala.Enumeration.Value"

val NONE = rtypes.TypeSymbolRType("<none>")

class ReflectException(msg: String) extends Exception(msg)