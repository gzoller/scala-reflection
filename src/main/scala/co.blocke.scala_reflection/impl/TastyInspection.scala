package co.blocke.scala_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.quoted.Quotes
import scala.tasty.inspector._
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

