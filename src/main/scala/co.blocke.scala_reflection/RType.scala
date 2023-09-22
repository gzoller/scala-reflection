package co.blocke.scala_reflection

import scala.quoted.*
import scala.reflect.ClassTag
import rtypes.*
import java.nio.file.{Files, Path}
import scala.tasty.inspector.TastyInspector


/**
  * RType trait that all the RTypes implement
  */
trait RType[R]:
  type T = R                // R is saved for accessibility during casting, ie myRType.asInstanceOf[fooRType.T]
  val name: String          // fully-qualified class name of this type
  val typedName: TypedName  // fully-qualified class name w/type parameters, if any, otherwise a copy of name
  lazy val clazz: Class[_]  // runtime class of this RType; lazy because computed at runtime

  // Stuff needed for equality tests, proper behavior in a Map, etc...
  override def hashCode: Int = name.hashCode
  override def equals(obj: Any) = this.hashCode == obj.hashCode

  def toType(quotes: Quotes): quoted.Type[T]  = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[Type[T]]
  def pretty(): String = Show.show(this)


/** This RType mixin needed because all AppliedTypes don't have parameters.  
 *  For examlpe a case class could be an applied type (isAppliedType=true) or not.  A collection is always applied.
 */
trait AppliedRType:
  self: RType[_] =>
  val typeParamSymbols: List[TypeSymbol]
  def isAppliedType: Boolean = true  // can be overridden to false, e.g. Scala class that isn't parameterized
  def select(i: Int): RType[_]
  def selectLimit: Int
  // Take a parameterized type's normal type 'T' and map it to the declared type 'X'
  def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[_]


// Marker trait denoting a primitive type
trait PrimitiveRType:
  self: RType[_] =>


//---------------------------------------------------------------------------------------


object RType:

  // pre-loaded cache with known language types including primitives
  protected[scala_reflection] val rtypeCache = scala.collection.mutable.Map.empty[TypedName, RType[_]] ++= PrimitiveRTypes.loadCache()


  //------------------------
  //  <<  MACRO ENTRY >>       (Tasty Reflection)
  //------------------------
  inline def of[T]: RType[T] = ${ ofImpl[T]() }

  def ofImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[RType[T]] =
    import quotes.reflect.*
    val rtype = unwindType(quotes)( TypeRepr.of[T] ).asInstanceOf[RType[T]]
    exprs.ExprMaster.makeExpr(rtype)

   
  //------------------------
  //  <<  NON-MACRO ENTRY >>  (Tasty Inspection)
  //------------------------
  def of(clazz: Class[_]): RType[_] = 
    import scala.quoted.staging.*
    import reflect.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    rtypeCache.getOrElse(clazz.getName, {
      val newRType = {
        val fn = (qctx: Quotes) ?=> {
          val newRType = RType.unwindType(qctx)(qctx.reflect.TypeRepr.typeConstructorOf(clazz), false)
          reflect.TypeSymbolFinder.mapTypeSymbolsForClass(newRType) // ClassRType with type parameter paths computed
        }
        withQuotes(fn)
      }
      rtypeCache.synchronized {
        rtypeCache.put(clazz.getName, newRType)
      }
      newRType
    })

  // Amazing way to call something (like TypeRepr.of) that needs quotes, when no quotes are availabe.  In this case str value isn't
  // known until runtime so it absolutely can't be passed in a macro (or boom!).  This bit o'magic materializes a quotes inside a 
  // function.  So you create the function, having code using quotes, then pass the function to the withQuotes() mirqacle, and viola!
  def inTermsOf(traitRType: RType[_], str: String): RType[_] = 
    import scala.quoted.staging._
    given Compiler = Compiler.make(getClass.getClassLoader)
    val fn = (quotes: Quotes) ?=> {
      import quotes.reflect.*

      // val classRType = of(Class.forName(str))
      // reflect.TypeSymbolFinder.findTypeSymbolsFor(classRType)

      val classQuotedTypeRepr = quotes.reflect.TypeRepr.typeConstructorOf(Class.forName(str))

      // val c = classQuotedTypeRepr.classSymbol.get.primaryConstructor

      // val y = c.caseFields
      // println("HEY: "+classQuotedTypeRepr.baseClasses)
      // println("-------")

      implicit val tt = traitRType.toType(quotes)
      // println("You: "+TypeRepr.of[traitRType.T].typeArgs.mkString("\n"))
      val traitsParams = TypeRepr.of[traitRType.T] match { 
        case AppliedType(_, traitsParams) => 
          val z = traitsParams
          z.map(p => println("Param: "+p.toString))
          z
      }
      println("Found: "+traitsParams.size)
      println(traitsParams)
      RType.unwindType(quotes)(classQuotedTypeRepr.appliedTo( traitsParams ))
    }
    withQuotes(fn)
  
  // ------------------------
  //   Common entry point of all reflection/inspection, to unwind the given type and return an RType[T]
  // ------------------------
  protected[scala_reflection] def unwindType[T](quotes: Quotes)(aType: quotes.reflect.TypeRepr, resolveTypeSyms: Boolean = true): RType[T] =
    import quotes.reflect.*

    val className = aType.asInstanceOf[TypeRef] match {
      case AndType(_,_) => Clazzes.INTERSECTION_CLASS
      case OrType(_,_)  => Clazzes.UNION_CLASS
      case _: dotty.tools.dotc.core.Types.WildcardType => "scala.Any"
      case normal       => normal.classSymbol.get.fullName
    }

    this.synchronized {
      val tName = typeName(quotes)(aType)
      rtypeCache.getOrElse(tName, {
        rtypeCache.put(tName, SelfRefRType(tName.toString))
        val reflectedRType = reflect.ReflectOnType(quotes)(aType, tName, resolveTypeSyms)
        rtypeCache.put(tName, reflectedRType)
        reflectedRType
      }).asInstanceOf[RType[T]]

      /*
      cache.getOrElse(tName, {
        if className == "scala.Any" then
          TastyReflection.reflectOnType(quotes)(aType, tName, resolveTypeSyms)
        else
          cache.put(tName, SelfRefRType(className))
          val reflectedRType = TastyReflection.reflectOnType(quotes)(aType, tName, resolveTypeSyms)
          cache.put(tName, reflectedRType)
          reflectedRType
      })
      */
    }

  // Need a full name inclusive of type parameters and correcting for Enumeration's class name erasure.
  // This name is used for RType.equals so caching works.
  def typeName(quotes: Quotes)(aType: quotes.reflect.TypeRepr): TypedName =
    import quotes.reflect.{_, given}
    aType.asInstanceOf[TypeRef] match {
      case sym if aType.typeSymbol.flags.is(Flags.Param) => sym.name
      case AppliedType(t,tob) =>
        typeName(quotes)(t).toString + tob.map( oneTob => typeName(quotes)(oneTob.asInstanceOf[TypeRef])).mkString("[",",","]")
      case AndType(left, right) => Clazzes.INTERSECTION_CLASS + "[" + typeName(quotes)(left.asInstanceOf[TypeRef]) + "," + typeName(quotes)(right.asInstanceOf[TypeRef]) + "]"
      case OrType(left, right) => Clazzes.UNION_CLASS + "[" + typeName(quotes)(left.asInstanceOf[TypeRef]) + "," + typeName(quotes)(right.asInstanceOf[TypeRef]) + "]"
      case _: dotty.tools.dotc.core.Types.WildcardType => "unmapped"
      case _ => aType.classSymbol.get.fullName match {
        case Clazzes.ENUM_CLASS => aType.asInstanceOf[TypeRef].qualifier.asInstanceOf[quotes.reflect.TermRef].termSymbol.moduleClass.fullName
        case tn => tn
      }
    }