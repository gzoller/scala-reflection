package co.blocke.scala_reflection

import scala.quoted.*
import reflect.rtypeRefs.{ScalaClassRef, SelfRefRef, TraitRef, UnknownRef}

trait RType[R]:
  type T = R // R is saved for accessibility during casting, ie myRType.asInstanceOf[fooRType.T]
  val name: String
  val typedName: TypedName

  lazy val clazz = Class.forName(name)
  def toType(quotes: Quotes): quoted.Type[R] =
    quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[Type[R]]

  def pretty: String = util.Pretty(this)
  // Stuff needed for equality tests, proper behavior in a Map, etc...
  override def hashCode: Int = typedName.hashCode
  override def equals(obj: Any) = this.hashCode == obj.hashCode

object RType:

  protected[scala_reflection] val rtypeCache = scala.collection.mutable.Map.empty[TypedName, RType[_]]

  // -----------------------------
  //  <<  MACRO ENTRY: of[T] >>       (Tasty Reflection)
  // -----------------------------
  inline def of[T]: RType[T] = ${ ofImpl[T]() }

  def ofImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[RType[T]] =
    import quotes.reflect.*
    reflect.ReflectOnType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
    // val z = reflect.ReflectOnType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    // println("Z: " + z)
    // val e = z.expr
    // println("\n\n>> " + e.show)
    // e

  // ------------------------
  //  <<  NON-MACRO ENTRY >>  (Tasty Inspection)
  // ------------------------
  def of(className: String): RType[?] =
    import scala.quoted.staging.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    rtypeCache.getOrElse(
      className, {
        val newRType = {
          val fn = (quotes: Quotes) ?=> reflect.ReflectOnType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(Class.forName(className)), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
          run(fn)
        }.asInstanceOf[RType[?]]
        rtypeCache.synchronized {
          rtypeCache.put(className, newRType)
        }
        newRType
      }
    )

  // ------------------------
  //  <<  NON-MACRO ENTRY: inTermsOf >>  (Tasty Inspection)
  // ------------------------
  inline def inTermsOf[T](className: String): RType[?] =
    import scala.quoted.staging.*
    of[T] match {
      case traitRType: rtypes.TraitRType[?] =>
        given Compiler = Compiler.make(getClass.getClassLoader)
        val fn = (quotes: Quotes) ?=> {
          import quotes.reflect.*

          val seenBefore = scala.collection.mutable.Map.empty[TypedName, Boolean]

          // Get paths for types
          val t1 = System.currentTimeMillis()
          val clazz = Class.forName(className)
          val symbol = TypeRepr.typeConstructorOf(clazz).typeSymbol
          val typeSymbols = symbol.primaryConstructor.paramSymss.head.map(_.name.asInstanceOf[TypeSymbol])
          val classDef = symbol.tree.asInstanceOf[ClassDef]
          val paths = reflect.TypeSymbolMapper.mapTypeSymbolsForClass(quotes)(classDef, typeSymbols)(using seenBefore)
          val t2 = System.currentTimeMillis()

          // Now apply these paths against traitRType
          val t3 = System.currentTimeMillis()
          val typeParamTypes = reflect.TypeSymbolMapper.deepApply(paths, typeSymbols, traitRType)(using quotes)
          val classQuotedTypeRepr = TypeRepr.typeConstructorOf(clazz)
          val q = reflect.ReflectOnType(quotes)(classQuotedTypeRepr.appliedTo(typeParamTypes))(using seenBefore).expr
          val t4 = System.currentTimeMillis()
          println("Class Path Time: " + (t2 - t1))
          println("Run Path Time  : " + (t4 - t3))
          q
        }
        run(fn)
      case x => throw new ReflectException(s"${x.name} is not of type trait")
    }

  // ------------------------------
  //  <<  MACRO ENTRY: ScalaJS >> (EXPERIMENTAL)
  // ------------------------------
  inline def ofJS[T]: String = ${ ofJSImpl[T]() }

  def ofJSImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[String] =
    import quotes.reflect.*
    val ref = reflect.ReflectOnType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
    val sb = new StringBuilder()
    ref.asJson(sb)
    Expr(sb.toString)
