package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

case class UnionRType[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    leftType: RType[?],
    rightType: RType[?]
) extends RType[R]
    with LeftRightRType[R]:

  val typedName: TypedName = name + "[" + leftType.typedName + "," + rightType.typedName + "]"
  def typeParamValues: List[RType[?]] = List(leftType, rightType)
  override lazy val clazz: Class[?] = Clazzes.AnyClazz

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val leftParamType: quoted.Type[leftType.T] = leftType.toType(quotes)
    val rightParamType: quoted.Type[rightType.T] = rightType.toType(quotes)
    val leftParamTypeRepr = TypeRepr.of[leftType.T](using leftParamType)
    val rightParamTypeRepr = TypeRepr.of[rightType.T](using rightParamType)
    OrType(leftParamTypeRepr, rightParamTypeRepr).asType.asInstanceOf[quoted.Type[R]]
