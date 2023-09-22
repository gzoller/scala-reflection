package co.blocke.scala_reflection
package rtypes

import scala.quoted.*

trait ClassRType[R] extends RType[R] with AppliedRType: 
  val name:                       String
  lazy val fields:                List[FieldInfo]
  val typeParamSymbols:           List[TypeSymbol]
  lazy val typeMembers:           List[TypeMemberRType]
  lazy val annotations:           Map[String, Map[String,String]]
  lazy val mixins:                List[String]

  def select(i: Int): RType[_] = 
    if i >= 0 && i < fields.size then
      fields(i).fieldType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}") 


case class ScalaClassRType[R] (
    name:                   String,
    typedName:              TypedName,
    typeParamSymbols:       List[TypeSymbol],
    _typeMembers:           List[TypeMemberRType],
    _fields:                List[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    override val isAppliedType: Boolean,
    isValueClass:           Boolean,
    isCaseClass:            Boolean,
    typeParamPaths:         List[List[Int]] = Nil
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

  def selectLimit: Int = _fields.size

  def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[R] =
    this
    // this.copy( 
    //   _fields = _fields.map( _.asInstanceOf[ScalaFieldInfo].resolveTypeParams(paramMap) )
    //   )

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
  // lazy val fields = _fields.map( f => f.fieldType match {
  //   // case s: SelfRefRType => f.asInstanceOf[ScalaFieldInfo].copy(fieldType = s.resolve)
  //   case s => f
  // })


  // The old inTermsOf().  This takes concrete type parameter values from a trait and applies them to this
  // parameterized class having unmapped types (all type symbols).  So Foo[T] >> FooTrait[Int] = Foo[Int]
  def >>( traitRT: TraitRType[_]): RType[_] = 
    import scala.quoted.staging.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    val me = this
    val fn = (quotes: Quotes) ?=> {
      import quotes.reflect.*
      val traitType = traitRT.toType(quotes)
      val typeParamTypes = reflect.TypeSymbolMapper.deepApply( me )(using quotes)(using traitType)
      val classQuotedTypeRepr = TypeRepr.typeConstructorOf(clazz)
      RType.unwindType(quotes)(classQuotedTypeRepr.appliedTo( typeParamTypes ))
    }
    withQuotes(fn)