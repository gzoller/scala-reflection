package co.blocke.scala_reflection

import scala.quoted._
import quoted.Quotes
import impl._
import info._
import java.io._
import java.nio.ByteBuffer
import scala.tasty.inspector.TastyInspector

/** A materializable type */
trait RType extends Serializable:
  val name: String         /** typically the fully-qualified class name */
  val fullName: String     /** fully-qualified name w/type parameters (if AppliedType, else a copy of name) */
  override def hashCode: Int = fullName.hashCode
  override def equals(obj: Any) = this.hashCode == obj.hashCode
  lazy val infoClass: Class[_]  /** the JVM class of this type */

  def toBytes( bbuf: ByteBuffer ): Unit
  def toType(quotes: Quotes): quotes.reflect.TypeRepr  = quotes.reflect.TypeRepr.typeConstructorOf(infoClass)

  def show(
    tab: Int = 0,
    seenBefore: List[String] = Nil,
    supressIndent: Boolean = false,
    modified: Boolean = false // modified is "special", ie. don't show index & sort for nonconstructor fields
    ): String  

  override def toString(): String = show()

  /** Write the object to a Base64 string. */
  def serialize: String =
    val buffer = ByteBuffer.allocate(BUFFER_MAX)
    toBytes(buffer)
    java.util.Base64.getEncoder().encodeToString(buffer.array.slice(0, buffer.position))


/** Needed because just because something is an AppliedType doesn't mean it has parameters.  For examlpe a case class could be
  *  an applied type (isAppliedType=true) or not.  A collection is always applied.
  */
trait AppliedRType:
  self: RType =>
  def isAppliedType: Boolean = true  // can be overridden to false, e.g. Scala class that isn't parameterized
  def select(i: Int): RType
  // Take a parameterized type's normal type 'T' and map it to the declared type 'X'
  def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType


// Poked this here for now.  Used for show()
final inline def tabs(t:Int) = "   "*t


//-------------------------------------------------------------------------------------------------------------------

trait RTypeOf:
  def of(clazz: Class[_]): RType
  def descendParents(clazz: Class[_]): Map[String, Map[String, List[Int]]]


class RTypeOfNoPlugin() extends RTypeOf:
  import scala.quoted.staging._ 
  given Compiler = Compiler.make(getClass.getClassLoader)

  def of(clazz: Class[_]): RType = 
    RType.cache.getOrElse(clazz.getName,
      RType.unpackAnno(clazz).getOrElse{
        val tc = new TastyInspection(clazz)
        val tastyPath = Option(clazz.getProtectionDomain.getCodeSource).map(src => src.getLocation.getPath + clazz.getName.replace(".","/") + ".tasty")
        tastyPath match {
          case Some(path) if ( new java.io.File(path) ).exists =>
            TastyInspector.inspectTastyFiles(List(path))(tc)
            tc.inspected
          case _ =>
            // Non-Tasty top-level class (Java, or String-marshalled Class)
            val fn = (qctx: Quotes) ?=> RType.unwindType(qctx)( qctx.reflect.TypeRepr.typeConstructorOf(clazz), false )
            val reflectedRType = withQuotes(fn)
            // dotty/staging/src/scala/quoted/staging/staging.scala:35:  def withQuotes[T](thunk: Quotes ?=> T)(using toolbox: Toolbox): T
            // >>> Expect withQuotes to accept a thunk and return an RType (RType.unwindType returns RType)
            RType.cache.synchronized {
              RType.cache.put(clazz.getName, reflectedRType)
            }
            reflectedRType
        }
      }
    )

  def descendParents(clazz: Class[_]): Map[String, Map[String, List[Int]]] =
    val fn = (qctx: Quotes) ?=> TypeLoom.descendParents(qctx)( qctx.reflect.TypeRepr.typeConstructorOf(clazz) ) 
    withQuotes(fn)


class RTypeOfWithPlugin(quotes: Quotes) extends RTypeOf:
  def of(clazz: Class[_]): RType = 
    RType.cache.getOrElse(clazz.getName,
      RType.unpackAnno(clazz).getOrElse{
        val tc = new TastyInspection(clazz)
        val tastyPath = clazz.getProtectionDomain.getCodeSource.getLocation.getPath + clazz.getName.replace(".","/") + ".tasty"
        if ( new java.io.File(tastyPath) ).exists then
          TastyInspector.inspectTastyFiles(List(tastyPath))(tc)
          tc.inspected
        else
          // Non-Tasty top-level class (Java, or String-marshalled Class)
          val reflectedRType = RType.unwindType(quotes)( quotes.reflect.TypeRepr.typeConstructorOf(clazz), false )
          RType.cache.synchronized {
            RType.cache.put(clazz.getName, reflectedRType)
          }
          reflectedRType
      }
    )

  def descendParents(clazz: Class[_]): Map[String, Map[String, List[Int]]] =
    TypeLoom.descendParents(quotes)( quotes.reflect.TypeRepr.typeConstructorOf(clazz) ) 

//-------------------------------------------------------------------------------------------------------------------


object RType:

  //------------------------
  //  <<  MACRO ENTRY >>
  //------------------------
  inline def of[T]: RType = ${ ofImpl[T]() }
  
  def ofImpl[T]()(implicit qctx: Quotes, ttype: scala.quoted.Type[T]): Expr[RType] = 
    import qctx.reflect.{_, given}
    Expr( unwindType(qctx)( TypeRepr.of[T]) )

  // This is preset if used as a compiler plugin
  var ofMethod: Option[RTypeOf] = None 

  def of(clazz: Class[_]): RType = (ofMethod match {
    case Some(om) => om
    case None => 
      ofMethod = Some(new RTypeOfNoPlugin())
      ofMethod.get
  }).of(clazz)
  
  inline def inTermsOf[T](clazz: Class[_]): RType = 
    inTermsOf(clazz, of[T].asInstanceOf[TraitInfo])

  inline def inTermsOf(clazz: Class[_], ito: TraitInfo): RType = 
    cache.get(clazz.getName) match {
      case Some(scib: ScalaClassInfoBase) => 
        scib.paths.get(ito.name) match {
          case Some(paths) => scib.resolveTypeParams( TypeLoom.Recipe.navigate( paths, ito ))
          case None        => scib
        }
      case None => 
        unpackAnno(clazz) match {
          case Some(scib: ScalaClassInfoBase) => 
            scib.paths.get(ito.name) match {
              case Some(paths) => scib.resolveTypeParams( TypeLoom.Recipe.navigate( paths, ito ))
              case None        => scib
            }
            
          case None =>
            val clazzRType = of(clazz)
            if !clazzRType.asInstanceOf[ClassInfo].isParameterized then
              clazzRType 
            else
              val symPaths = (ofMethod match {
                case Some(om) => om
                case None => 
                  ofMethod = Some(new RTypeOfNoPlugin())
                  ofMethod.get
              }).descendParents(clazz)
              symPaths.get(ito.name) match {
                case Some(paths) => clazzRType.asInstanceOf[ScalaClassInfoBase].resolveTypeParams( TypeLoom.Recipe.navigate( paths, ito ))
                case None        => clazzRType
              }
              
          case _ => 
            throw new ReflectException(s"ClassInfo in annotation for ${clazz.getName} is not a Scala 3 class")
        }
      case _ =>
        throw new ReflectException(s"Cached class ${clazz.getName} is not a Scala 3 class")
    }

  inline def unpackAnno(c: Class[_]): Option[RType] =
    c.getAnnotations.toList.collectFirst{
      case s3r: S3Reflection => RType.deserialize(s3r.rtype)
    }

  // pre-loaded with known language primitive types
  val cache = scala.collection.mutable.Map.empty[String,RType] ++ PrimitiveType.loadCache
  def cacheSize = cache.size

  protected[scala_reflection] def unwindType(quotes: Quotes)(aType: quotes.reflect.TypeRepr, resolveTypeSyms: Boolean = true): RType =
    import quotes.reflect.{_, given}

    val className = aType.asInstanceOf[TypeRef] match {
      case AndType(_,_) => INTERSECTION_CLASS
      case OrType(_,_)  => UNION_CLASS
      case _: dotty.tools.dotc.core.Types.WildcardType => "scala.Any"
      case normal       => normal.classSymbol.get.fullName
    }

    this.synchronized {
      val tName = typeName(quotes)(aType)
      cache.getOrElse(tName, { 
        if className == "scala.Any" then
          TastyReflection.reflectOnType(quotes)(aType, tName, resolveTypeSyms)
        else
          cache.put(tName, SelfRefRType(className))
          val reflectedRType = TastyReflection.reflectOnType(quotes)(aType, tName, resolveTypeSyms)
          cache.put(tName, reflectedRType)
          reflectedRType
      })
    }

  // Need a full name inclusive of type parameters and correcting for Enumeration's class name erasure.
  // This name is used for RType.equals so caching works.
  def typeName(quotes: Quotes)(aType: quotes.reflect.TypeRepr): String =
    import quotes.reflect.{_, given}
    val name = aType.asInstanceOf[TypeRef] match {
      case sym if aType.typeSymbol.flags.is(Flags.Param) => sym.name
      case AppliedType(t,tob) => 
        typeName(quotes)(t) + tob.map( oneTob => typeName(quotes)(oneTob.asInstanceOf[TypeRef])).mkString("[",",","]")
      case AndType(left, right) => INTERSECTION_CLASS + "[" + typeName(quotes)(left.asInstanceOf[TypeRef]) + "," + typeName(quotes)(right.asInstanceOf[TypeRef]) + "]"
      case OrType(left, right) => UNION_CLASS + "[" + typeName(quotes)(left.asInstanceOf[TypeRef]) + "," + typeName(quotes)(right.asInstanceOf[TypeRef]) + "]"
      case _: dotty.tools.dotc.core.Types.WildcardType => "unmapped"
      case _ => aType.classSymbol.get.fullName
    }
    name match {
      case ENUM_CLASSNAME => aType.asInstanceOf[TypeRef].qualifier.asInstanceOf[quotes.reflect.TermRef].termSymbol.moduleClass.fullName
      case tn => tn
    }

  def fromBytes( bbuf: ByteBuffer ): RType = 
    bbuf.get() match {
      case SCALA_BOOLEAN         => PrimitiveType.Scala_Boolean
      case SCALA_DOUBLE          => PrimitiveType.Scala_Double
      case SCALA_INT             => PrimitiveType.Scala_Int
      case SCALA_LONG            => PrimitiveType.Scala_Long 
      case SCALA_STRING          => PrimitiveType.Scala_String
      case SCALA_ANY             => PrimitiveType.Scala_Any
      case SELFREF               => SelfRefRType.fromBytes(bbuf)
      case ALIAS_INFO            => AliasInfo.fromBytes(bbuf)
      case SCALA_CASE_CLASS_INFO => ScalaCaseClassInfo.fromBytes(bbuf)
      case SCALA_CLASS_INFO      => ScalaClassInfo.fromBytes(bbuf)
      case JAVA_CLASS_INFO       => JavaClassInfo.fromBytes(bbuf)
      case JAVA_CLASS_INFO_PROXY => JavaClassInfoProxy.fromBytes(bbuf)
      case SEQLIKE_INFO          => SeqLikeInfo.fromBytes(bbuf)
      case MAPLIKE_INFO          => MapLikeInfo.fromBytes(bbuf)
      case ARRAY_INFO            => ArrayInfo.fromBytes(bbuf)
      case EITHER_INFO           => EitherInfo.fromBytes(bbuf)
      case ENUM_INFO             => ScalaEnumInfo.fromBytes(bbuf)
      case ENUMERATION_INFO      => ScalaEnumerationInfo.fromBytes(bbuf)
      case JAVA_ENUM_INFO        => JavaEnumInfo.fromBytes(bbuf)
      case INTERSECTION_INFO     => IntersectionInfo.fromBytes(bbuf)
      case OBJECT_INFO           => ObjectInfo.fromBytes(bbuf)
      case OPTION_INFO           => ScalaOptionInfo.fromBytes(bbuf)
      case OPTIONAL_INFO         => JavaOptionalInfo.fromBytes(bbuf)
      case SCALA2_INFO           => Scala2Info.fromBytes(bbuf)
      case TRAIT_INFO            => TraitInfo.fromBytes(bbuf)
      case SEALED_TRAIT_INFO     => SealedTraitInfo.fromBytes(bbuf)
      case TRY_INFO              => TryInfo.fromBytes(bbuf)
      case TUPLE_INFO            => TupleInfo.fromBytes(bbuf)
      case TYPE_MEMBER_INFO      => TypeMemberInfo.fromBytes(bbuf)
      case TYPE_SYMBOL_INFO      => TypeSymbolInfo.fromBytes(bbuf)
      case UNION_INFO            => UnionInfo.fromBytes(bbuf)
      case UNKNOWN_INFO          => UnknownInfo.fromBytes(bbuf)
      case SCALA_BYTE            => PrimitiveType.Scala_Byte
      case SCALA_CHAR            => PrimitiveType.Scala_Char
      case SCALA_FLOAT           => PrimitiveType.Scala_Float
      case SCALA_SHORT           => PrimitiveType.Scala_Short
      case JAVA_SET_INFO         => JavaSetInfo.fromBytes(bbuf)
      case JAVA_LIST_INFO        => JavaListInfo.fromBytes(bbuf)
      case JAVA_ARRAY_INFO       => JavaArrayInfo.fromBytes(bbuf)
      case JAVA_QUEUE_INFO       => JavaQueueInfo.fromBytes(bbuf)
      case JAVA_STACK_INFO       => JavaStackInfo.fromBytes(bbuf)
      case JAVA_MAP_INFO         => JavaMapInfo.fromBytes(bbuf)
      case JAVA_BOOLEAN          => PrimitiveType.Java_Boolean
      case JAVA_BYTE             => PrimitiveType.Java_Byte
      case JAVA_CHAR             => PrimitiveType.Java_Char
      case JAVA_DOUBLE           => PrimitiveType.Java_Double
      case JAVA_FLOAT            => PrimitiveType.Java_Float
      case JAVA_INT              => PrimitiveType.Java_Int
      case JAVA_LONG             => PrimitiveType.Java_Long
      case JAVA_SHORT            => PrimitiveType.Java_Short
      case JAVA_OBJECT           => PrimitiveType.Java_Object
      case JAVA_NUMBER           => PrimitiveType.Java_Number
    }

  def deserialize( s: String ): RType =
    val data = java.util.Base64.getDecoder().decode( s )
    val bbuf = ByteBuffer.wrap(data)
    fromBytes(bbuf)
  