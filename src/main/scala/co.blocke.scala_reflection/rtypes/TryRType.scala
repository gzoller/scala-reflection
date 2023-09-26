package co.blocke.scala_reflection
package rtypes

import scala.util.Try
import scala.quoted.Quotes


case class TryRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _tryType: RType[_]
) extends RType[R] with AppliedRType:

  val typedName: TypedName = name + "[" + _tryType.typedName  + "]"
  def selectLimit: Int = 1
  lazy val clazz: Class[_] = Class.forName(name)
  lazy val tryType: RType[_] = _tryType 

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val tryType: quoted.Type[R] = super.toType(quotes)
    val paramType: quoted.Type[_tryType.T] = _tryType.toType(quotes)
    val tryTypeRepr = TypeRepr.of[R](using tryType)
    val paramTypeRepr = TypeRepr.of[_tryType.T](using paramType)
    AppliedType(tryTypeRepr, List(paramTypeRepr)).asType.asInstanceOf[quoted.Type[R]]


  def select(i: Int): RType[_] = 
    if i == 0 then
      _tryType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")