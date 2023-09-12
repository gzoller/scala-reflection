package co.blocke.scala_reflection

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Annotations.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.Pickler
import dotty.tools.dotc.quoted.*

import scala.quoted.runtime.impl.QuotesImpl
import dotty.tools.backend.sjs.GenSJSIR
import info.*


/** Compiler plugin (phase) that will tap into any TypeDefs, and identify any classes being defined.
 *  It will then pre-reflect on that class and serialized the resultant RType into an annotation.
 *  This saves 2-6 sec of "priming" time when reflecting on a class using Tasty Inspection (runtime).
 */

class ReflectionWorker extends StandardPlugin {
  val name: String = "reflectionWorker"
  override val description: String = "heavy-lift reflection worker"

  def init(options: List[String]): List[PluginPhase] =
    new ReflectionWorkerPhase :: Nil
}

class ReflectionWorkerPhase extends PluginPhase {
  import tpd._

  val phaseName = "reflectionWorker"

  override val runsAfter = Set(Pickler.name)

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree =
    if tree.isClassDef && !tree.rhs.symbol.isStatic then  // only look at classes & traits, not objects
      // Reflect on the type (generate an RType), then serialize to string and add the S3Reflection annotation to the class.
      val quotes = QuotesImpl.apply()

      RType.ofMethod = Some(new RTypeOfWithPlugin(quotes))

      val unpackedType = tree.tpe.classSymbol.appliedRef.asInstanceOf[quotes.reflect.TypeRepr]
      val reflected = RType.unwindType(quotes)(unpackedType,false)
      val s3ReflectionClassSymbol = getClassIfDefined("co.blocke.scala_reflection.S3Reflection")
      val annoArg = NamedArg("rtype".toTermName, Literal(quotes.reflect.StringConstant( reflected.serialize ).asInstanceOf[dotty.tools.dotc.core.Constants.Constant]))
      tree.symbol.addAnnotation(Annotation.apply(s3ReflectionClassSymbol.asInstanceOf[ClassSymbol], annoArg, tree.span) )
    tree
}

/* Playing around with JS phase...
class JSWorkerPhase extends PluginPhase {
  import tpd._

  val phaseName = "reflectionWorkerJS"

  override val runsAfter = Set(GenSJSIR.name)

  override def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree =
    if tree.isClassDef && !tree.rhs.symbol.isStatic && ctx.settings.scalajs.value then  // only look at classes & traits, not objects
      println("scala.js is enabled!")
      val s3ReflectionClassSymbol = getClassIfDefined("co.blocke.scala_reflection.S3Reflection").asInstanceOf[ClassSymbol]
      val anno = tree.symbol.getAnnotation(s3ReflectionClassSymbol)
      anno.map{ rt =>
        val rtypeArg = rt.arguments(0)
        rtypeArg match {
          case NamedArg(_,Literal(Constant(c))) => println(RType.deserialize(c.toString))
        }
        println("isIt? "+(tree.getClass.getName))
        println("Tree: "+tree.tpe)
      }
    tree
}
*/

/*
Handy way to see compiler phases:

$ scalac -Xshow-phases
    phase name  id  description
    ----------  --  -----------
        parser   1  parse source into ASTs, perform simple desugaring
         namer   2  resolve names, attach symbols to named trees
packageobjects   3  load package objects
         typer   4  the meat and potatoes: type the trees
        patmat   5  translate match expressions
superaccessors   6  add super accessors in traits and nested classes
    extmethods   7  add extension methods for inline classes
       pickler   8  serialize symbol tables
     refchecks   9  reference/override checking, translate nested objects
       uncurry  10  uncurry, translate function values to anonymous classes
        fields  11  synthesize accessors and fields, add bitmaps for lazy vals
     tailcalls  12  replace tail calls by jumps
    specialize  13  @specialized-driven class and method specialization
 explicitouter  14  this refs to outer pointers
       erasure  15  erase types, add interfaces for traits
   posterasure  16  clean up erased inline classes
    lambdalift  17  move nested functions to top level
  constructors  18  move field definitions into constructors
       flatten  19  eliminate inner classes
         mixin  20  mixin composition
       cleanup  21  platform-specific cleanups, generate reflective calls
    delambdafy  22  remove lambdas
           jvm  23  generate JVM bytecode
      terminal  24  the last phase during a compilation run
*/