package co.blocke.scala_reflection
package reflect

import rtypes.UnknownRType
import scala.quoted.*
import scala.tasty.inspector.*

/** This class is needed for runtime inspection--so we can get a Reflection object to pass to unwindType
  */
class TastyInspection(clazz: Class[?]) extends Inspector:

  var inspected: RType[?] = UnknownRType(clazz.getName)

  // Note: Class arg to TastyInspection drives inspection, NOT tastys List.
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*
    val tastyType = quotes.reflect.TypeRepr.typeConstructorOf(clazz)
    inspected = RType.unwindType(quotes)(tastyType)
