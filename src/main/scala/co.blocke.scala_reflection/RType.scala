package co.blocke.scala_reflection

import scala.quoted.*
import scala.reflect.ClassTag
import rtypes.*
import reflect.*
import java.nio.file.{Files, Path}
import scala.tasty.inspector.TastyInspector

/** RType trait that all the RTypes implement
  */
trait RType[R]:
  type T = R // R is saved for accessibility during casting, ie myRType.asInstanceOf[fooRType.T]
  val name: String // fully-qualified class name of this type
  val typedName: TypedName // fully-qualified class name w/type parameters, if any, otherwise a copy of name
  lazy val clazz: Class[?] // runtime class of this RType; lazy because computed at runtime

  // Stuff needed for equality tests, proper behavior in a Map, etc...
  override def hashCode: Int = name.hashCode
  override def equals(obj: Any) = this.hashCode == obj.hashCode
  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit // Used for ScalaJS support

  def toType(quotes: Quotes): quoted.Type[T] =
    quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[Type[T]]
  def pretty: String = Show.show(this)

/** This RType mixin needed because all AppliedTypes don't have parameters.
  *  For examlpe a case class could be an applied type (isAppliedType=true) or not.  A collection is always applied.
  */
trait AppliedRType:
  self: RType[?] =>
  val typeParamSymbols: List[TypeSymbol]
  def isAppliedType: Boolean = true // can be overridden to false, e.g. Scala class that isn't parameterized
  def select(i: Int): RType[?]
  def selectLimit: Int

// Marker trait denoting a primitive type
trait PrimitiveRType:
  self: RType[?] =>

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "PrimitiveRType"),
        JsonField("name", self.name)
      )
    )

//---------------------------------------------------------------------------------------

object RType:

  // pre-loaded cache with known language types including primitives
  protected[scala_reflection] val rtypeCache =
    scala.collection.mutable.Map.empty[TypedName, RType[_]] ++= PrimitiveRTypes.loadCache()

  // ------------------------
  //  <<  MACRO ENTRY >>       (Tasty Reflection)
  // ------------------------
  inline def of[T]: RType[T] = ${ ofImpl[T]() }

  def ofImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[RType[T]] =
    import quotes.reflect.*
    val rtype = unwindType(quotes)(TypeRepr.of[T]).asInstanceOf[RType[T]]
    exprs.ExprMaster.makeExpr(rtype)

  // ------------------------------
  //  <<  MACRO ENTRY: ScalaJS >> (EXPERIMENTAL)
  // ------------------------------
  inline def ofJS[T]: String = ${ ofJSImpl[T]() }

  def ofJSImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[String] =
    import quotes.reflect.*
    val rtype = unwindType(quotes)(TypeRepr.of[T]).asInstanceOf[RType[T]]
    val sb = new StringBuilder()
    rtype.asJson(sb)
    Expr(sb.toString)

  // ------------------------
  //  <<  NON-MACRO ENTRY >>  (Tasty Inspection)
  // ------------------------
  def of(className: String): RType[?] =
    import scala.quoted.staging.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    rtypeCache.getOrElse(
      className, {
        val newRType = {
          val fn = (quotes: Quotes) ?=> RType.unwindType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(Class.forName(className)))
          withQuotes(fn)
        }
        rtypeCache.synchronized {
          rtypeCache.put(className, newRType)
        }
        newRType
      }
    )

  inline def inTermsOf[T](clazz: ScalaClassRType[?]): RType[?] =
    of[T] match {
      case t: TraitRType[?] => clazz >> t
      case x                => throw new ReflectException(s"${x.name} is not of type trait")
    }

  // ------------------------
  //   Common entry point of all reflection/inspection, to unwind the given type and return an RType[T]
  // ------------------------
  protected[scala_reflection] def unwindType[T](
      quotes: Quotes
  )(aType: quotes.reflect.TypeRepr, resolveTypeSyms: Boolean = true): RType[T] =
    import quotes.reflect.*

    val className = aType.asInstanceOf[TypeRef] match {
      case AndType(_, _)                               => Clazzes.INTERSECTION_CLASS
      case OrType(_, _)                                => Clazzes.UNION_CLASS
      case _: dotty.tools.dotc.core.Types.WildcardType => "scala.Any"
      case normal                                      => normal.classSymbol.get.fullName
    }

    this.synchronized {
      val tName = typeName(quotes)(aType)
      rtypeCache
        .getOrElse(
          tName,
          if className == Clazzes.ANY_CLASS then
            ReflectOnType(quotes)(
              aType,
              tName,
              resolveTypeSyms
            ) // Reflect on Any.  Could be Any, or an opaque type usage
          else
            rtypeCache.put(tName, SelfRefRType(className, tName))
            val reflectedRType = ReflectOnType(quotes)(aType, tName, resolveTypeSyms)
            rtypeCache.put(tName, reflectedRType)
            reflectedRType
        )
        .asInstanceOf[RType[T]]
    }

  // var cnt = 0
  // Need a full name inclusive of type parameters and correcting for Enumeration's class name erasure.
  // This name is used for RType.equals so caching works.
  def typeName(quotes: Quotes)(aType: quotes.reflect.TypeRepr): TypedName =
    import quotes.reflect.*

    aType.asInstanceOf[TypeRef] match {
      case AppliedType(t, tob) =>
        t match {
          case AppliedType(t2, tob2) =>
            typeName(quotes)(t2).toString + tob2
              .map(oneTob => typeName(quotes)(oneTob.asInstanceOf[TypeRef]))
              .mkString("[", ",", "]")
          case _ =>
            typeName(quotes)(t).toString + tob
              .map(oneTob => typeName(quotes)(oneTob.asInstanceOf[TypeRef]))
              .mkString("[", ",", "]")
        }
      case sym if aType.typeSymbol.flags.is(Flags.Param) =>
        sym.name
      case AndType(left, right) =>
        Clazzes.INTERSECTION_CLASS + "[" + typeName(quotes)(left.asInstanceOf[TypeRef]) + "," + typeName(quotes)(
          right.asInstanceOf[TypeRef]
        ) + "]"
      case OrType(left, right) =>
        Clazzes.UNION_CLASS + "[" + typeName(quotes)(left.asInstanceOf[TypeRef]) + "," + typeName(quotes)(
          right.asInstanceOf[TypeRef]
        ) + "]"
      case _: dotty.tools.dotc.core.Types.WildcardType =>
        "unmapped"
      case _ =>
        aType.classSymbol.get.fullName match {
          case Clazzes.ENUMERATION_CLASS =>
            aType.asInstanceOf[TypeRef].qualifier.asInstanceOf[quotes.reflect.TermRef].termSymbol.moduleClass.fullName
          case tn =>
            tn
        }
    }
