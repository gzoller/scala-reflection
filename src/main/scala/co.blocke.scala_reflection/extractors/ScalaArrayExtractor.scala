package co.blocke.scala_reflection
package extractors

import rtypes.Clazzes.*
import rtypes.*
import reflect.TypeExtractor
import scala.quoted.Quotes
import scala.util.Try

case class ScalaArrayExtractor() extends TypeExtractor[ArrayRType[_]]:

  def matches(quotes: Quotes)(symbol: quotes.reflect.Symbol): Boolean =
    // Try here because non-library symbol won't have a class and will explode.
    scala.util.Try(ScalaArrayClazz.isAssignableFrom(Class.forName(symbol.fullName))).toOption.getOrElse(false)

  def extractInfo(
      quotes: Quotes
  )(t: quotes.reflect.TypeRepr, tob: List[quotes.reflect.TypeRepr], symbol: quotes.reflect.Symbol): RType[?] =

    val arrayOfType = tob.head
    val isTypeParam = arrayOfType.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val typeParamSymbols = List("A")
    val arrayOfRType =
      if isTypeParam then TypeSymbolRType(tob.head.typeSymbol.name)
      else RType.unwindType(quotes)(tob.head)

    val mangled = mangleArrayClassName(arrayOfRType)
    ArrayRType(mangled, typeParamSymbols, arrayOfRType)

  private def mangleArrayClassName(tpe: co.blocke.scala_reflection.RType[?]): String =
    val mangled = tpe match {
      case _: TypeSymbolRType[?] => "Ljava.lang.Object;"
      case c: ArrayRType[?]      => mangleArrayClassName(c.elementType)
      // case c: rtypes.JavaArrayInfo => mangleArrayClassName(c.elementType)
      case _: BooleanRType => "Z"
      case _: ByteRType    => "B"
      case _: CharRType    => "C"
      case _: DoubleRType  => "D"
      case _: FloatRType   => "F"
      case _: IntRType     => "I"
      case _: LongRType    => "J"
      case _: ShortRType   => "S"
      case _: AnyRType     => "Ljava.lang.Object;"
      case c               => "L" + c.name + ";"
    }
    "[" + mangled
