package co.blocke.scala_reflection
package rtypes

import scala.quoted.*

trait ClassRType[R] extends RType[R] with AppliedRType: 
  val name:                       String
  lazy val fields:                List[FieldInfo]
  val typeParamSymbols:           List[TypeSymbol]
  val typeParamValues:            List[RType[_]]
  lazy val annotations:           Map[String, Map[String,String]]
  lazy val mixins:                List[String]

  def selectLimit: Int = typeParamSymbols.size
  def select(i: Int): RType[_] = 
    if i >= 0 && i < selectLimit then
      typeParamValues(i)
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")   


// Convenience for creating a ScalaClassRType, often for use with '>>' operator.
object ScalaClassRType:
  def apply(className: String): ScalaClassRType[_] = RType.of(className) match {
    case s: ScalaClassRType[_] => s
    case _ => throw new ReflectException(s"$className is not a Scala class")
  }


//------------------------------------------------------------------------------


case class ScalaClassRType[R] (
    name:                   String,
    typedName:              TypedName,
    typeParamSymbols:       List[TypeSymbol],
    typeParamValues:        List[RType[_]],      // Like Int, Boolean
    _typeMembers:           List[TypeMemberRType],
    _fields:                List[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    override val isAppliedType: Boolean,
    isValueClass:           Boolean,
    isCaseClass:            Boolean,
    isAbstractClass:        Boolean,
    typeParamPaths:         Map[String,List[List[Int]]] = Map.empty[String,List[List[Int]]], // Trait/Class name -> List of Int (path) for each type param
    nonConstructorFields:   List[NonConstructorFieldInfo] = Nil,  // Populated for non-case classes only
    sealedChildren:         List[RType[_]] = Nil // Populated only if this is a sealed class or abstract class
) extends ClassRType[R]:

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val classType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]]
    val classTypeRepr = TypeRepr.of[R](using classType)
    val fieldTypes = _fields.map{ f => 
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(classTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]

  override def equals(obj: Any) =
    obj match {
      case s: ScalaClassRType[_] if s.name == this.name => s._fields.toList == this.fields.toList
      case _ => false
    }

  // All this laziness is because Java classes use a proxy that isn't resolved until runtime.
  lazy val typeMembers = _typeMembers
  lazy val annotations = _annotations
  lazy val mixins = _mixins
  lazy val clazz: Class[_] = {
    try {
      Class.forName(name)
    } catch {
      case cnfe: ClassNotFoundException => {
        if (name.contains("$.")) {
          Class.forName(name.replace("$.", "$"))
        } else {
          throw cnfe
        }
      }
    }
  }
  lazy val typeParams = clazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields

  // Convenience for RType.inTermsOf().  This takes concrete type parameter values from a trait and applies them to this
  // parameterized class having unmapped types (all type symbols).  So Foo[T] >> FooTrait[Int] = Foo[Int]
  def >>( traitRT: TraitRType[_]): RType[_] = 
    import scala.quoted.staging.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    val me = this
    val fn = (quotes: Quotes) ?=> {
      import quotes.reflect.*
      val typeParamTypes = reflect.TypeSymbolMapper.deepApply( me, traitRT )(using quotes)
      val classQuotedTypeRepr = TypeRepr.typeConstructorOf(clazz)
      RType.unwindType(quotes)(classQuotedTypeRepr.appliedTo( typeParamTypes ))
    }
    withQuotes(fn)


//------------------------------------------------------------------------------



/** Java class reflection has a special problem... we need the class file, which isn't available during compilation (i.e. inside a macro).
 *  The best we can do is capture the name of the class and materialize/reflect on the class outside of the macro, lazy-like.
 */

case class JavaClassRType[R] ( 
    name:               String, 
    _fields:            List[FieldInfo],
    typeParamSymbols:   List[TypeSymbol],
    typeParamValues:    List[RType[_]],
    _annotations:       Map[String, Map[String,String]],
    _mixins:            List[String],
    classType:          Option[Type[R]] = None   // Internal use only! (fixes broken Classloader for Java classes inside a macro)
  ) extends ClassRType[R]:
    
  val typedName: TypedName = name
  lazy val fields: List[FieldInfo] = _fields
  lazy val annotations = _annotations
  lazy val mixins = _mixins

  lazy val clazz: Class[_] = Class.forName(name)

  // private lazy val proxy =
  //   _proxy.getOrElse(impl.JavaClassInspector.inspectClass(infoClass, fullName, paramTypes).asInstanceOf[JavaClassInfoProxy])
  // lazy val fields                                                 = proxy.fields
  // lazy val typeMembers:           Array[TypeMemberInfo]           = proxy.typeMembers
  // lazy val annotations:           Map[String, Map[String,String]] = proxy.annotations
  // lazy val mixins:                Array[String]                   = proxy.mixins
  override def isAppliedType: Boolean = !typeParamSymbols.isEmpty

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val classType: quoted.Type[R] = this.classType.getOrElse(quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]])
    val classTypeRepr = TypeRepr.of[R](using classType)
    val fieldTypes = _fields.map{ f => 
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(classTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]


  // def field(name: String): Option[JavaFieldInfo] = fieldsByName.get(name)

  // private lazy val fieldsByName = fields.map(f => (f.name, f.asInstanceOf[JavaFieldInfo])).toMap


  // def show(tab:Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
  //   val newTab = {if suppressIndent then tab else tab+1}

  //   if seenBefore.contains(name) then
  //     {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
  //   else
  //     {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName
  //     + s"($name):\n"
  //     + tabs(newTab) + "fields:\n" + fields.map(_.show(newTab+1,name :: seenBefore)).mkString
  //     + {if annotations.nonEmpty then tabs(newTab) + "annotations: "+annotations.toString + "\n" else ""}


  /*
case class JavaClassInfoProxy protected[scala_reflection] (
    name:                   String,
    _fields:                Array[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    paramMap:               Map[TypeSymbol, RType]
  ) extends RType:

  val typedName: TypedName = name
  lazy val annotations = _annotations
  lazy val infoClass: Class[_] = Class.forName(name)

  // Run up the interitance tree to the top (Object) to get all the superclasses and mixin interfaces of this one
  private def getSuperclasses(c: Class[_] = infoClass, stack:List[String] = List.empty[String]): List[String] = 
    val ammendedStack = (stack :+ c.getName) ::: c.getInterfaces.toList.map(_.getName)
    val sc = c.getSuperclass()
    if( sc == classOf[Object] || sc == null)
      ammendedStack
    else 
      getSuperclasses(sc, ammendedStack)

  lazy val mixins = getSuperclasses().toArray
 

  // Fields may be self-referencing, so we need to unwind this...
  lazy val fields = _fields.map{ f => 
    val fieldType = f.fieldType match {
      case s: SelfRefRType => f.asInstanceOf[JavaFieldInfo].copy(fieldType = s.resolve)
      case s => f
    }
    fieldType.resolveTypeParams(paramMap)
  }

  val typeMembers: Array[TypeMemberInfo] = Nil.toArray  // unused for Java classes but needed on ClassInfo

  def show(tab:Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
    {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"

  def toBytes( bbuf: ByteBuffer ): Unit = 
    bbuf.put( JAVA_CLASS_INFO_PROXY )
    StringByteEngine.write(bbuf, name)
    ArrayFieldInfoByteEngine.write(bbuf, _fields)
    MapStringByteEngine.write(bbuf, _annotations)
    MapStringRTypeByteEngine.write(bbuf, paramMap.asInstanceOf[Map[String,RType]])

  def jsSerialize(sb: StringBuffer): Unit =
    sb.append(s"""{"kind":"Java class (proxy)","name":"$name","fullName":"$fullName","_fields":""")
    RType.jsListSerialize(sb, _fields.toSeq, (buf:StringBuffer, v:FieldInfo)=>v.jsSerialize(buf))
    sb.append(""","_annotations":""")
    RType.jsMapSerialize(
      sb,
      _annotations,
      (buf: StringBuffer, v: Map[String, String]) => RType.jsMapSerialize(buf, v, (sb2: StringBuffer, v2: String) => sb2.append(s""""$v2""""))
    )
    sb.append(""","paramMap":""")
    RType.jsMapSerialize(
      sb,
      paramMap.asInstanceOf[Map[String,RType]],
      (buf: StringBuffer, v: RType) => v.jsSerialize(buf)
    )
    sb.append("}")
    */