package co.blocke.scala_reflection
package impl

import scala.tasty.Reflection

trait TypeInfoExtractor[T <: RType]:

  def matches(reflect: Reflection)(symbol: reflect.Symbol): Boolean

  def extractInfo(reflect: Reflection)(
    t: reflect.TypeRepr, 
    tob: List[reflect.TypeRepr], 
    symbol: reflect.Symbol): RType

