package co.blocke.scala_reflection
package rtypes

import scala.quoted.*

trait ClassRType[R] extends RType[R] with AppliedRType:
  val name: String
  val fields: List[FieldInfo]
  val typeParamSymbols: List[TypeSymbol]
  val typeParamValues: List[RType[_]]
  val annotations: Map[String, Map[String, String]]
  val mixins: List[String]

//------------------------------------------------------------------------------

case class ScalaClassRType[R](
    name: String,
    typedName: TypedName,
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RType[_]], // Like Int, Boolean
    typeMembers: List[TypeMemberRType],
    fields: List[FieldInfo],
    annotations: Map[String, Map[String, String]],
    mixins: List[String],
    isAppliedType: Boolean,
    isValueClass: Boolean,
    isCaseClass: Boolean,
    isAbstractClass: Boolean,
    nonConstructorFields: List[NonConstructorFieldInfo] = Nil, // Populated for non-case classes only
    sealedChildren: List[RType[_]] = Nil // Populated only if this is a sealed class or abstract class
) extends ClassRType[R]:

  override def equals(obj: Any) =
    obj match {
      case s: ScalaClassRType[?] if s.name == this.name => s.fields.toList == this.fields.toList
      case _                                            => false
    }

//------------------------------------------------------------------------------

/** Java class reflection has a special problem... we need the class file, which isn't available during compilation (i.e. inside a macro).
  *  So we need an internal-use-only field (classType) where we store Type[T] for the Java class--which we know during reflection.
  */
case class JavaClassRType[R](
    name: String,
    fields: List[FieldInfo],
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RType[_]],
    annotations: Map[String, Map[String, String]],
    mixins: List[String]
) extends ClassRType[R]:

  val typedName: TypedName = name
