package co.blocke.scala_reflection
package impl

import info._
import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tools.dotc.ast.Trees.AppliedTypeTree
  
/** This class is needed for runtime inspection--so we can get a Reflection object to pass to unwindType
 */
class TastyInspection(clazz: Class[_]) extends TastyInspector:

  var inspected: RType = UnknownInfo(clazz.getName)
  var tasty: scala.tasty.Reflection = null
  var clazzType: dotty.tools.dotc.core.Types.CachedType = null

  protected def processCompilationUnit(using qctx: QuoteContext)(root: qctx.tasty.Tree): Unit =
    val tastyType = qctx.reflect.TypeRepr.typeConstructorOf(clazz)
    inspected = RType.unwindType(qctx.reflect)( tastyType, false )
    tasty = qctx.reflect
    clazzType = tastyType.asInstanceOf[dotty.tools.dotc.core.Types.CachedType]
