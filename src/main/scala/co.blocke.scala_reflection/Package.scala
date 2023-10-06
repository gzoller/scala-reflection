package co.blocke.scala_reflection

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String
given listOstring2TypeSymbol: Conversion[List[String], List[TypeSymbol]] with
  def apply(x: List[String]): List[TypeSymbol] = x.asInstanceOf[List[TypeSymbol]]

/** Class name also having any type parameters listed (if any) */
opaque type TypedName = String
given string2typedname: Conversion[String, TypedName] with
  def apply(x: String): TypedName = x.asInstanceOf[TypedName]

class ReflectException(msg: String) extends Exception(msg)
