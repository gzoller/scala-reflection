package co.blocke.scala_reflection
package impl

import info._
import scala.tasty.Reflection

/** Marker trait for all Scala/Java collections */
trait CollectionRType extends AppliedRType:
  self: RType =>

  lazy val elementType: RType

  override def toType(reflect: Reflection): reflect.Type = 
    import reflect.{_, given _}
    implicit val stuff = reflect.rootContext.asInstanceOf[dotty.tools.dotc.core.Contexts.Context] 
    dotty.tools.dotc.core.Types.AppliedType(
      Type.typeConstructorOf(self.infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(elementType.toType(reflect).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[reflect.AppliedType]

  def select(i: Int): RType = 
    if i == 0 then
      elementType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")

  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    {if(!supressIndent) tabs(tab) else ""} + this.getClass.getSimpleName 
    + s"($name): "
    + elementType.show(newTab,name :: seenBefore,true)