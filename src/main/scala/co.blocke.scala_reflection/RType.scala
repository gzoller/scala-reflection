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
  /*
  inline def myMacro: Any = ${ myMacroExpr('{1}) }

def myMacroExpr(x: Expr[Int])(using Quotes): Expr[Any] =
  '{
    def f(using q1: Quotes) = '{ 1 + ${Expr($x)} }
    ()
  }


  inline def visitExportsExprMap[T](inline x: T)(inline f: T => Any): Any = ${ visitExportsExprMapImpl('x, 'f) }
  private def visitExportsExprMapImpl[T: Type](e: Expr[T], f: Expr[T => Any])(using Quotes): Expr[Any] =
    '{$f(${IdempotentExprMap.transform(e)})}

  def makeClass( cn: String ): Class[?] => Class.forName(cn)
~*/

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
  // def of(className: String): RType[?] =
  //   rtypeCache.getOrElse(
  //     className, {
  //       val newRType = {
  //         val fn = (quotes: Quotes) ?=> reflect.ReflectOnType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(Class.forName(className)), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
  //         run(fn)
  //       }.asInstanceOf[RType[?]]
  //       rtypeCache.synchronized {
  //         rtypeCache.put(className, newRType)
  //       }
  //       newRType
  //     }
  //   )

  inline def of(className: String): RType[?] = ${ ofImpl('className) }

  def ofImpl(className: Expr[String])(using q: Quotes): Expr[RType[?]] =
    import quotes.reflect.*
    '{
      reflect.ReflectOnType(q)(q.reflect.TypeRepr.typeConstructorOf(Class.forName($className)), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).toRType
    }
    // val makeThingFn = '{ (q2: Quotes, cn: String) =>
    //   reflect.ReflectOnType(q2)(q2.reflect.TypeRepr.typeConstructorOf(Class.forName(cn)), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
    // }
    // '{
    //   val cn = $className
    //   val clz = $makeThingFn(q2, $className)
    //   ${ reflect.ReflectOnType(q)(q.reflect.TypeRepr.typeConstructorOf(Class.forName(('cn).value.get)), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr }
    // }

  /*
I'm trying to untangle something...  I'm struggling getting things I need at the right staging level.
```scala
  inline def examineClass(className: String): ResultRecor = ${ examineClassImpl('className) }

  def examineClassImpl(className: Expr[String])(using q: Quotes): Expr[RType[?]] =
    import quotes.reflect.*
    // At this point (compile-time), className.value is null--not set until runtime stage in '{}
    '{
      val cn = $className  // At this point $className has a value, i.e. className.value.get is non-null
      ${ reflectOnClass(q)(q.reflect.TypeRepr.typeConstructorOf(Class.forName(cn))) }
    }
```
I need to run with:
```scala
val cname = "com.foo.MyClass"
examineClass(cname)
```

You can see my problem...  I need to get className into the ${} block.
   */

  // ------------------------
  //  <<  NON-MACRO ENTRY: inTermsOf >>  (Tasty Inspection)
  // ------------------------
  inline def mapTypesForSymbols[T](className: String): Map[TypeSymbol, RType[?]] =
    val t1 = System.currentTimeMillis()
    val traitRType = of[T].asInstanceOf[RType[_]]
    val t2 = System.currentTimeMillis()
    val classRType = of(className).asInstanceOf[rtypes.ScalaClassRType[_]]
    val t3 = System.currentTimeMillis()
    val pathsForTrait = classRType.typePaths.getOrElse(traitRType.name, throw new ReflectException(s"No path in class ${classRType.name} for trait ${traitRType.name}"))
    val t4 = System.currentTimeMillis()
    val z = classRType.typeParamSymbols
      .zip(
        pathsForTrait.map(onePath =>
          onePath.foldLeft(traitRType)((rt, i) =>
            rt match
              case art: AppliedRType => art.select(i)
              case _                 => throw new ReflectException("Non-AppliedType found where AppliedType expected")
          )
        )
      )
      .toMap
    val t5 = System.currentTimeMillis()
    println("TIME: " + (t2 - t1) + " / " + (t3 - t2) + " / " + (t4 - t3) + " / " + (t5 - t4))
    z

  /*
  inline def inTermsOf[T](className: String): RType[?] =
    of[T] match {
      case traitRType: rtypes.TraitRType[?] =>
        val classRType = of(className).asInstanceOf[rtypes.ScalaClassRType[_]]

        val i: Int = (Math.random() * 100).toInt
        val fn = (quotes: Quotes) ?=> {
          import quotes.reflect.*

          val seenBefore = scala.collection.mutable.Map.empty[TypedName, Boolean]

          // def deepApply2(quotes: Quotes)(path: List[List[Int]], traitTypeRepr: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr]
          val t1 = System.currentTimeMillis()
          val paths = classRType.typePaths.getOrElse(traitRType.name, throw new ReflectException(s"No path in class ${classRType.name} for trait ${traitRType.name}"))
          implicit val tt = traitRType.toType(quotes)
          val typeParamTypes = reflect.TypeSymbolMapper.runPath(quotes)(paths, TypeRepr.of[traitRType.T])
          val t2 = System.currentTimeMillis()

          // Apply type param paths from classRType against traitRType
          // val typeParamTypes = reflect.TypeSymbolMapper.deepApply(classRType.typePaths, classRType.typeParamSymbols, traitRType)(using quotes)
          val classQuotedTypeRepr = TypeRepr.typeConstructorOf(classRType.clazz)
          val z = reflect.ReflectOnType(quotes)(classQuotedTypeRepr.appliedTo(typeParamTypes))(using seenBefore).expr
          val t3 = System.currentTimeMillis()
          z
        }
        val t4 = System.currentTimeMillis()
        val q = run(fn)
        val t5 = System.currentTimeMillis()
        println(s"($i) Total: " + (t5 - t4))
        q
      // So.... the whole "run()" machinery roughly doubles the total processing time of the fn() body... :-(
      case x => throw new ReflectException(s"${x.name} is not of type trait")
    }
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
