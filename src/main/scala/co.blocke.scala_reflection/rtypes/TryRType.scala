package co.blocke.scala_reflection
package rtypes

import scala.util.Try
import scala.quoted.Quotes


case class TryRType[R] (
  name: String,
  _tryType: RType[_]
) extends RType[R] with AppliedRType:

  val typedName: TypedName = name + "[" + _tryType.typedName  + "]"
  lazy val clazz: Class[_] = Class.forName(name)
  lazy val tryType: RType[_] = _tryType match {
    // case e: SelfRefRType => e.resolve
    case e => e
  }

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
      
  // def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
  //   val newTab = {if suppressIndent then tab else tab+1}
  //   {if(!suppressIndent) tabs(tab) else ""} + s"Try of " + tryType.show(newTab,name :: seenBefore,true)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[_] = this
    // _tryType match {
    //   case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => TryInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
    //   case art: AppliedRType if art.isAppliedType => TryInfo(name, art.resolveTypeParams(paramMap))
    //   case _ => this
    // }
