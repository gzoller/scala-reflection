package co.blocke.scala_reflection

import scala.quoted.*
import rtypeRefs.{ScalaClassRef, SelfRefRef, TraitRef, UnknownRef}

trait RType[R]:
  type T = R // R is saved for accessibility during casting, ie myRType.asInstanceOf[fooRType.T]
  val name: String
  val typedName: TypedName

  def pretty: String = Pretty(this)
  // Stuff needed for equality tests, proper behavior in a Map, etc...
  override def hashCode: Int = typedName.hashCode
  override def equals(obj: Any) = this.hashCode == obj.hashCode

object RType:

  protected[scala_reflection] val rtypeCache = scala.collection.mutable.Map.empty[TypedName, RType[_]]

  // -----------------------------
  //  <<  MACRO ENTRY: of[T] >>       (Tasty Reflection)
  // -----------------------------
  // inline def of[T]: String = ${ ofImpl[T]() }
  inline def of[T]: RType[T] = ${ ofImpl[T]() }

  def ofImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[RType[T]] =
    import quotes.reflect.*
    reflect.ReflectOnType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
    // val z = exploreType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    // println("Z: " + z)
    // val y = z.expr
    // '{ $y.toString }

  // ------------------------
  //  <<  NON-MACRO ENTRY >>  (Tasty Inspection)
  // ------------------------
  def of(className: String): RType[?] =
    import scala.quoted.staging.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    rtypeCache.getOrElse(
      className, {
        val newRType = {
          val fn = (quotes: Quotes) ?=> reflect.ReflectOnType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(Class.forName(className)))(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
          run(fn)
        }.asInstanceOf[RType[?]]
        rtypeCache.synchronized {
          rtypeCache.put(className, newRType)
        }
        newRType
      }
    )

  inline def inTermsOf[T](clazzName: String): RType[T] = ${ inTermsOfImpl[T]('clazzName) }

  def inTermsOfImpl[T](classNameExpr: Expr[String])(using t: Type[T])(using quotes: Quotes): Expr[RType[T]] =
    import quotes.reflect.*
    val seenBefore = scala.collection.mutable.Map.empty[TypedName, Boolean]
    val traitRef = reflect.ReflectOnType[T](quotes)(TypeRepr.of[T])(using seenBefore).asInstanceOf[TraitRef[_]]
    val classTypeRepr = TypeRepr.typeConstructorOf(Class.forName(classNameExpr.value.get))
    val classRef = classTypeRepr.asType match
      case '[t] =>
        reflect.ReflectOnType[t](quotes)(classTypeRepr)(using seenBefore).asInstanceOf[ScalaClassRef[_]]
    val typeParamTypes = reflect.TypeSymbolMapper.deepApply(classRef, traitRef)(using quotes)
    val applied = classTypeRepr.appliedTo(typeParamTypes)
    applied.asType match
      case '[t] =>
        reflect.ReflectOnType[t](quotes)(applied)(using seenBefore).expr.asInstanceOf[Expr[RType[T]]]
