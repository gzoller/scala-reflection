package co.blocke.scala_reflection
package impl

import info.*
import extractors.*
import scala.quoted.*
import scala.reflect.*
import scala.quoted.Quotes
import scala.tasty.inspector.*
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  
/** This class is needed for runtime inspection--so we can get a Reflection object to pass to unwindType
 */
class TastyInspection(clazz: Class[_]) extends Inspector:

  var inspected: RType = UnknownInfo(clazz.getName)

  // Note: Class arg to TastyInspection drives inspection, NOT tastys List.
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect._
    val tastyType = quotes.reflect.TypeRepr.typeConstructorOf(clazz)
    inspected = RType.unwindType(quotes)( tastyType, false )

