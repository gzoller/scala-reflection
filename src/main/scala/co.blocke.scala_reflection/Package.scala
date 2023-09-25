package co.blocke.scala_reflection

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String 
given listOstring2TypeSymbol: Conversion[List[String], List[TypeSymbol]] with
  def apply(x: List[String]): List[TypeSymbol] = x.asInstanceOf[List[TypeSymbol]]

/** Class name also having any type parameters listed (if any) */
opaque type TypedName = String
given string2typedname: Conversion[String, TypedName] with
  def apply(x: String): TypedName = x.asInstanceOf[TypedName]

val NONE = rtypes.TypeSymbolRType("<none>")

class ReflectException(msg: String) extends Exception(msg)

def mangleArrayClassName(tpe: RType[_]): String =
  val mangled = tpe match {
    case _: rtypes.TypeSymbolRType => "Ljava.lang.Object;"
    case c: rtypes.ArrayRType[_] => mangleArrayClassName(c.elementType)
    // case c: rtypes.JavaArrayInfo => mangleArrayClassName(c.elementType)
    case _: rtypes.BooleanRType => "Z"
    case _: rtypes.ByteRType => "B"
    case _: rtypes.CharRType => "C"
    case _: rtypes.DoubleRType => "D"
    case _: rtypes.FloatRType => "F"
    case _: rtypes.IntRType => "I"
    case _: rtypes.LongRType => "J"
    case _: rtypes.ShortRType => "S"
    case _: rtypes.AnyRType => "Ljava.lang.Object;"
    case c => "L" + c.name + ";"
  }
  "[" + mangled


// Handy "break"-able fold iterator.  Return Right to stop/complete.
def foldLeftBreak[A, B](as: List[A])(init: B)(op: (A, B) => Either[B, B]): B =
  as match {
    case Nil => init
    case a :: as =>
      op(a, init) match {
        case Right(b) => b
        case Left(b) => foldLeftBreak(as)(b)(op)
      }
  }