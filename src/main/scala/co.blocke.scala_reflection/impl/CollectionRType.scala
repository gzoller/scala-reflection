package co.blocke.scala_reflection
package impl

import info.*
import scala.quoted.Quotes

/** Marker trait for all Scala/Java collections */
trait CollectionRType extends AppliedRType:
  self: RType =>

  lazy val elementType: RType

  override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
    import quotes.reflect.{_, given}
    implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx 
    dotty.tools.dotc.core.Types.AppliedType(
      TypeRepr.typeConstructorOf(self.infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(elementType.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[quotes.reflect.AppliedType]

  def select(i: Int): RType = 
    if i == 0 then
      elementType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")

  def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if suppressIndent then tab else tab+1}
    {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name): "
    + elementType.show(newTab,name :: seenBefore,true)