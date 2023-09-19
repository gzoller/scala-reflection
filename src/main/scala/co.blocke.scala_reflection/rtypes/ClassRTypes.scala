package co.blocke.scala_reflection
package rtypes

trait ClassRType[R] extends RType[R] with AppliedRType: 
  val name:                       String
  lazy val fields:                List[FieldInfo]
  val paramSymbols:               List[TypeSymbol]
  lazy val typeMembers:           List[TypeMemberRType]
  lazy val annotations:           Map[String, Map[String,String]]
  lazy val mixins:                List[String]

  def select(i: Int): RType[_] = 
    if i >= 0 && i <= fields.size-1 then
      fields(i).fieldType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}") 


case class ScalaClassRType[R] (
    name:                   String,
    typedName:              TypedName,
    paramSymbols:           List[TypeSymbol],
    _typeMembers:           List[TypeMemberRType],
    _fields:                List[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    paths:                  Map[String, Map[String,List[Int]]],  // <- TODO: Try to eliminate this and TypeLoom!  Replace w/AppliedType stuff...
    _mixins:                List[String],
    override val isAppliedType: Boolean,
    isValueClass:           Boolean,
    isCaseClass:            Boolean
) extends ClassRType[R]:

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
