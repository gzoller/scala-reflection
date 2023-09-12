package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

// case class BooleanRType() extends RType[Boolean]{  val name = "scala.Boolean" }
// case class ByteRType() extends RType[Byte]{ val name = "scala.Byte" }
// case class CharRType() extends RType[Char]{ val name = "scala.char" }
// case class DoubleRType() extends RType[Double]{ val name = "scala.Double" }
// case class FloatRType() extends RType[Float]{ val name = "scala.Float" }
// case class IntRType() extends RType[Int]{ val name = "scala.Int" }
// case class LongRType() extends RType[Long]{ val name = "scala.Double" }
// case class ShortRType() extends RType[Short]{ val name = "scala.Short" }
// case class StringRType() extends RType[String]{ val name = "java.lang.String" }
// case class AnyRType() extends RType[Any]{ val name = "scala.Any" }

object Primitives:

  def makeExpr[T](prt: PrimitiveRType)(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    prt match {
      case t: IntRType => '{ IntRType().asInstanceOf[RType[T]] }
        // Apply(
        //   Select.unique(New(TypeTree.of[IntRType]),"<init>"), 
        //   Nil
        // ).asExprOf[RType[T]]
      case t: StringRType => '{ StringRType().asInstanceOf[RType[T]] }
        // Apply(
        //   Select.unique(New(TypeTree.of[StringRType]),"<init>"), 
        //   Nil
        // ).asExprOf[RType[T]]
    }
