package co.blocke.scala_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.quoted.Quotes
import scala.tasty.inspector.TastyInspector
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  
/** This class is needed for runtime inspection--so we can get a Reflection object to pass to unwindType
 */
class TastyInspection(clazz: Class[_]) extends TastyInspector:

  var inspected: RType = UnknownInfo(clazz.getName)

  protected def processCompilationUnit(using quotes: Quotes)(root: quotes.reflect.Tree): Unit =
    val tastyType = quotes.reflect.TypeRepr.typeConstructorOf(clazz)
    inspected = RType.unwindType(quotes)( tastyType, false )
