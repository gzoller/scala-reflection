package co.blocke.scala_reflection

import scala.quoted.*
import reflect.rtypeRefs.{ScalaClassRef, SelfRefRef, TraitRef, UnknownRef}
import rtypes.{AppliedRType, TraitRType}

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
  import scala.quoted.staging.*
  given Compiler = Compiler.make(getClass.getClassLoader)

  protected[scala_reflection] val rtypeCache = scala.collection.mutable.Map.empty[TypedName, RType[_]]

  inline def echo(s: String): String = ${ echoImpl('s) }

  def wrap(q: Quotes)(t: Expr[String]): Expr[String] =
    import q.reflect.*
    implicit val q2 = q
    '{ "\"" + $t + "\"" }

  def echoImpl(s: Expr[String])(using q: Quotes): Expr[String] =
    import quotes.reflect.*
    // level 0 (compile time)
    '{ // level 1 (runtime)
      val s2 = $s
      "Hello, " + ${ // level 0
        wrap(q)(
          's2
        )
      }
    }

  // -----------------------------
  //  <<  MACRO ENTRY: of[T] >>       (Tasty Reflection)
  // -----------------------------
  inline def of[T]: RType[T] = ${ ofImpl[T]() }

  def ofImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[RType[T]] =
    import quotes.reflect.*
    reflect.ReflectOnType[T](quotes)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr

  // ------------------------
  //  <<  NON-MACRO ENTRY >>  (Tasty Inspection)
  // ------------------------
  def of(clazz: Class[?]): RType[?] =
    rtypeCache.getOrElse(
      clazz.getName, {
        val newRType = {
          val fn = (quotes: Quotes) ?=> reflect.ReflectOnType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(clazz), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
          run(fn)
        }.asInstanceOf[RType[?]]
        rtypeCache.synchronized {
          rtypeCache.put(clazz.getName, newRType)
        }
        newRType
      }
    )

  // ------------------------
  //  <<  NON-MACRO ENTRY: inTermsOf >>  (Tasty Inspection)
  // ------------------------
  inline def inTermsOf[T](clazz: Class[?]): RType[?] =
    of[T] match {
      case traitRType: rtypes.TraitRType[?] =>
        val classRType = of(clazz).asInstanceOf[rtypes.ScalaClassRType[_]]

        val i: Int = (Math.random() * 100).toInt
        val fn = (quotes: Quotes) ?=> {
          import quotes.reflect.*

          val seenBefore = scala.collection.mutable.Map.empty[TypedName, Boolean]

          // def deepApply2(quotes: Quotes)(path: List[List[Int]], traitTypeRepr: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr]
          val paths = classRType.typePaths.getOrElse(traitRType.name, throw new ReflectException(s"No path in class ${classRType.name} for trait ${traitRType.name}"))
          implicit val tt = traitRType.toType(quotes)
          val typeParamTypes = reflect.TypeSymbolMapper.runPath(quotes)(paths, TypeRepr.of[traitRType.T])

          // Apply type param paths from classRType against traitRType
          // val typeParamTypes = reflect.TypeSymbolMapper.deepApply(classRType.typePaths, classRType.typeParamSymbols, traitRType)(using quotes)
          val classQuotedTypeRepr = TypeRepr.typeConstructorOf(classRType.clazz)
          reflect.ReflectOnType(quotes)(classQuotedTypeRepr.appliedTo(typeParamTypes))(using seenBefore).expr
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
