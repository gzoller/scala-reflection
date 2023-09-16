package co.blocke.scala_reflection

import scala.quoted.*
import scala.reflect.ClassTag
import rtypes.*

trait RType[R]:
  type T = R                // R is saved for accessibility during casting, ie myRType.asInstanceOf[fooRType.T]
  val name: String          // fully-qualified class name of this type
  val typedName: TypedName  // fully-qualified class name w/type parameters, if any, otherwise a copy of name
  lazy val clazz: Class[_]
  override def hashCode: Int = name.hashCode
  override def equals(obj: Any) = this.hashCode == obj.hashCode
  def toType(quotes: Quotes): quoted.Type[T]  = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[Type[T]]
  def prettyPrint(): String = Show.show(this)

//   def show(
//     tab: Int = 0,
//     seenBefore: List[String] = Nil,
//     suppressIndent: Boolean = false,
//     modified: Boolean = false // modified is "special", ie. don't show index & sort for nonconstructor fields
//   ): String = infoClass.getClass.getName
//   override def toString(): String = show()


/** This RType mixin needed because j
 * ust because something is an AppliedType doesn't mean it has parameters.  
 *  For examlpe a case class could be an applied type (isAppliedType=true) or not.  A collection is always applied.
 */
trait AppliedRType:
  self: RType[_] =>
  def isAppliedType: Boolean = true  // can be overridden to false, e.g. Scala class that isn't parameterized
  def select(i: Int): RType[_]
  // Take a parameterized type's normal type 'T' and map it to the declared type 'X'
  def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[_]


// Marker trait denoting a primitive type
trait PrimitiveRType:
  self: RType[_] =>


object RType:

  // pre-loaded cache with known language types including primitives
  protected[scala_reflection] val rtypeCache = scala.collection.mutable.Map.empty[TypedName, RType[_]] ++= PrimitiveRTypes.loadCache()


  //------------------------0
  //  <<  MACRO ENTRY >>
  //------------------------
  inline def of[T]: RType[T] = ${ ofImpl[T]() }

  def ofImpl[T]()(using t: Type[T])(using quotes: Quotes): Expr[RType[T]] =
    import quotes.reflect.*
    val rtype = unwindType(quotes)( TypeRepr.of[T] ).asInstanceOf[RType[T]]
    exprs.ExprMaster.makeExpr(rtype)

   
  //------------------------
  //  <<  NON-MACRO ENTRY >>
  //------------------------
//   def of(clazz: Class[_]): RType[_] = RTypeOfNoPlugin().of(clazz)


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
        val reflectedRType = reflect.TastyReflection.reflectOnType(quotes)(aType, tName, resolveTypeSyms)
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