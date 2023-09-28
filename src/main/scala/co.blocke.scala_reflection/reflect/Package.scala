package co.blocke.scala_reflection.reflect

import co.blocke.scala_reflection.rtypes.*
import scala.quoted.*


val NONE = TypeSymbolRType("<none>")


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


inline def annoSymToString(quotes: Quotes)( terms: List[quotes.reflect.Term] ): Map[String,String] =
  import quotes.reflect._
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