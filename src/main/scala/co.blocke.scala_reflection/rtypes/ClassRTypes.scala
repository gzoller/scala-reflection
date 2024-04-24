package co.blocke.scala_reflection
package rtypes

import scala.quoted.*

trait ClassRType[R] extends RType[R] with AppliedRType:
  val name: String
  val fields: List[FieldInfo]
  val typeParamSymbols: List[TypeSymbol]
  val typeParamValues: List[RType[?]]
  val annotations: Map[String, Map[String, String]]
  val mixins: List[String]

//------------------------------------------------------------------------------

case class ScalaClassRType[R](
    name: String,
    typedName: TypedName,
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RType[?]], // Like Int, Boolean
    typeMembers: List[TypeMemberRType],
    fields: List[FieldInfo],
    annotations: Map[String, Map[String, String]],
    mixins: List[String],
    isAppliedType: Boolean,
    isValueClass: Boolean,
    isCaseClass: Boolean,
    isAbstractClass: Boolean,
    typePaths: Map[String, List[List[Int]]], // pre-computed path to each type symbol used (to get concrete types)
    nonConstructorFields: List[NonConstructorFieldInfo] = Nil, // Populated for non-case classes only
    sealedChildren: List[RType[?]] = Nil, // Populated only if this is a sealed class or abstract class
    childrenAreObject: Boolean = false
) extends ClassRType[R]:

  override def equals(obj: Any) =
    obj match {
      case s: ScalaClassRType[?] if s.name == this.name => s.fields.toList == this.fields.toList
      case _                                            => false
    }

  override lazy val clazz = Class.forName(util.AdjustClassName(name))

  def isSealed: Boolean = sealedChildren.nonEmpty

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val classType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]]
    val classTypeRepr = TypeRepr.of[R](using classType)
    val fieldTypes = fields.map { f =>
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(classTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]

//------------------------------------------------------------------------------

/** Java class reflection has a special problem... we need the class file, which isn't available during compilation (i.e. inside a macro).
  *  So we need an internal-use-only field (classType) where we store Type[T] for the Java class--which we know during reflection.
  */
case class JavaClassRType[R](
    name: String,
    fields: List[FieldInfo],
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RType[?]],
    annotations: Map[String, Map[String, String]],
    mixins: List[String]
) extends ClassRType[R]:

  val typedName: TypedName = name

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val classType: quoted.Type[?] =
      quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType
    val classTypeRepr = TypeRepr.of[R](using classType.asInstanceOf[quoted.Type[R]])
    val fieldTypes = fields.map { f =>
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(classTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]
