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

// This silly little piece of drama is sadly necessary to keep Scala's ADHD type-checker happy.
// We need to take the incoming RType (z), which has some given type, and explicitly cast it to
// RType[_] to make Scala happy.  Sigh.  It works great when we do, so...
inline def stripType(z: scala.quoted.Expr[RType[_]])(using q: scala.quoted.Quotes): scala.quoted.Expr[RType[_]] =
  '{ $z.asInstanceOf[RType[_]] }
