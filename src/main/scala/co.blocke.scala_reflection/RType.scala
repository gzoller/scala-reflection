package co.blocke.scala_reflection

import scala.quoted.*
import rtypeRefs.{ScalaClassRef, SelfRefRef, TraitRef, UnknownRef}

trait RType[R]:
  type T = R // R is saved for accessibility during casting, ie myRType.asInstanceOf[fooRType.T]
  val name: String
  val typedName: TypedName

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
    // z.expr

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

  inline def echo(inline msg: String): String = ${ echoImpl('{ msg }) }

  def echoImpl(msgExpr: Expr[String])(using quotes: Quotes): Expr[String] =
    import quotes.reflect.*
    // '{ "Hello!  Your message is: " + $msgExpr }
    Expr("Hello!  Your message is: " + msgExpr.value.get)

  def ito[T](className: String): RType[?] =
    import scala.quoted.staging.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    val newRType = {
      val fn = (quotes: Quotes) ?=> {
        import quotes.reflect.*

        val seenBefore = scala.collection.mutable.Map.empty[TypedName, Boolean]

        val r = TypeRepr.of[T]
        // val ctx = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
        // val scope = ctx.scope.asInstanceOf[scala.quoted.runtime.impl.Scope]
        // val typeTree = TypeTree.of[T].asInstanceOf[dotty.tools.dotc.ast.tpd.Tree]
        // val tt = new scala.quoted.runtime.impl.TypeImpl(typeTree, scope).asInstanceOf[Type[T]]

        // import scala.reflect.runtime.universe.*
        // implicit val tt: TypeTag[T] = implicitly[TypeTag[T]]
        // implicit val z: Type[T] = Type.of[T]

        val traitRef = reflect.ReflectOnType[T](quotes)(TypeRepr.of[T](using Type.of[T]))(using seenBefore).asInstanceOf[TraitRef[T]]

        val classTypeRepr = TypeRepr.typeConstructorOf(Class.forName(className))
        val classRef = classTypeRepr.asType match
          case '[t] =>
            reflect.ReflectOnType[t](quotes)(classTypeRepr)(using seenBefore).asInstanceOf[ScalaClassRef[_]]
        val typeParamTypes = reflect.TypeSymbolMapper.deepApply(classRef, traitRef)(using quotes)
        val applied = classTypeRepr.appliedTo(typeParamTypes)
        applied.asType match
          case '[t] =>
            reflect.ReflectOnType[t](quotes)(applied)(using seenBefore).expr.asInstanceOf[Expr[RType[T]]]
      }
      run[RType[T]](fn)
    }.asInstanceOf[RType[?]]
    if !rtypeCache.contains(className) then
      rtypeCache.synchronized {
        rtypeCache.put(className, newRType)
      }
    newRType

  // ------------------------------
  //  <<  MACRO ENTRY: inTermsOf >>
  // ------------------------------
  inline def inTermsOf[T](clazzName: String): RType[T] = ${ inTermsOfImpl[T]('clazzName) }

  // def unpack(str: Expr[String])(using quotes: Quotes): Expr[String] =
  //   '{ $str }

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

  /*
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
   */

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
