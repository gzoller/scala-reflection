package co.blocke.scala_reflection

// import scala.quoted.Quotes
import scala.quoted.{Expr, Quotes, Type}

/** Mnemonic symbol for a type--typically a paramaterized type, e.g. Foo[T], where T is the symbol */
opaque type TypeSymbol = String
given listOstring2TypeSymbol: Conversion[List[String], List[TypeSymbol]] with
  def apply(x: List[String]): List[TypeSymbol] = x.asInstanceOf[List[TypeSymbol]]

/** Class name also having any type parameters listed (if any) */
opaque type TypedName = String
given string2typedname: Conversion[String, TypedName] with
  def apply(x: String): TypedName = x.asInstanceOf[TypedName]

class ReflectException(msg: String) extends Exception(msg)

val NONE = "<none>"

val NEOTYPE = "neotype.Newtype"

def annoSymToString(quotes: Quotes)(terms: List[quotes.reflect.Term]): Map[String, String] =
  import quotes.reflect.*
  terms.collect {
    case NamedArg(argName, Literal(BooleanConstant(argValue))) => (argName -> argValue.toString)
    case NamedArg(argName, Literal(ByteConstant(argValue)))    => (argName -> argValue.toString)
    case NamedArg(argName, Literal(ShortConstant(argValue)))   => (argName -> argValue.toString)
    case NamedArg(argName, Literal(CharConstant(argValue)))    => (argName -> argValue.toString)
    case NamedArg(argName, Literal(IntConstant(argValue)))     => (argName -> argValue.toString)
    case NamedArg(argName, Literal(LongConstant(argValue)))    => (argName -> argValue.toString)
    case NamedArg(argName, Literal(FloatConstant(argValue)))   => (argName -> argValue.toString)
    case NamedArg(argName, Literal(DoubleConstant(argValue)))  => (argName -> argValue.toString)
    case NamedArg(argName, Literal(StringConstant(argValue)))  => (argName -> argValue)
  }.toMap

// Handy "break"-able fold iterator.  Return Right to stop/complete.
def foldLeftBreak[A, B](as: List[A])(init: B)(op: (A, B) => Either[B, B]): B =
  as match {
    case Nil => init
    case a :: as =>
      op(a, init) match {
        case Right(b) => b
        case Left(b)  => foldLeftBreak(as)(b)(op)
      }
  }

def ofOption[T](xs: Option[Expr[T]])(using Type[T])(using q: Quotes): Expr[Option[T]] =
  import q.reflect.*
  if xs.isEmpty then '{ None }
  else '{ Some(${ xs.get }) }
