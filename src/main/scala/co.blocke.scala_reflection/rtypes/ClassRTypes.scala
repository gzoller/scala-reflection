package co.blocke.scala_reflection
package rtypes

import scala.quoted.*
import reflect.{JsonField, JsonObjectBuilder}

trait ClassRType[R] extends RType[R] with AppliedRType:
  val name: String
  val fields: List[FieldInfo]
  val typeParamSymbols: List[TypeSymbol]
  val typeParamValues: List[RType[_]]
  val annotations: Map[String, Map[String, String]]
  val mixins: List[String]

  def selectLimit: Int = typeParamSymbols.size
  def select(i: Int): RType[?] =
    if i >= 0 && i < selectLimit then typeParamValues(i)
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

// Convenience for creating a ScalaClassRType, often for use with '>>' operator.
object ScalaClassRType:
  def apply(className: String): ScalaClassRType[?] = RType.of(className) match {
    case s: ScalaClassRType[?] => s
    case _                     => throw new ReflectException(s"$className is not a Scala class")
  }

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
    override val isAppliedType: Boolean,
    isValueClass: Boolean,
    isCaseClass: Boolean,
    isAbstractClass: Boolean,
    typeParamPaths: Map[String, List[List[Int]]] = Map.empty[String, List[List[Int]]], // Trait/Class name -> List of Int (path) for each type param
    nonConstructorFields: List[NonConstructorFieldInfo] = Nil, // Populated for non-case classes only
    sealedChildren: List[RType[_]] = Nil // Populated only if this is a sealed class or abstract class
) extends ClassRType[R]:

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val classType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]]
    val classTypeRepr = TypeRepr.of[R](using classType)
    val fieldTypes = fields.map { f =>
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(classTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]

  override def equals(obj: Any) =
    obj match {
      case s: ScalaClassRType[?] if s.name == this.name => s.fields.toList == this.fields.toList
      case _                                            => false
    }

  lazy val clazz: Class[?] =
    try
      Class.forName(name)
    catch {
      case cnfe: ClassNotFoundException =>
        if name.contains("$.") then {
          Class.forName(name.replace("$.", "$"))
        } else {
          throw cnfe
        }
    }
  lazy val typeParams = clazz.getTypeParameters.toList.map(_.getName.asInstanceOf[TypeSymbol])

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ScalaClassRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("typeParamValues", this.typeParamValues),
        JsonField("typeMembers", this.typeMembers),
        JsonField("fields", this.fields),
        JsonField("annotations", this.annotations),
        JsonField("mixins", this.mixins),
        JsonField("isAppliedType", this.isAppliedType),
        JsonField("isValueClass", this.isValueClass),
        JsonField("isCaseClass", this.isCaseClass),
        JsonField("isAbstractClass", this.isAbstractClass),
        JsonField("nonConstructorFields", this.nonConstructorFields),
        JsonField("sealedChildren", this.sealedChildren)
      )
    )

  // Convenience for RType.inTermsOf().  This takes concrete type parameter values from a trait and applies them to this
  // parameterized class having unmapped types (all type symbols).  So Foo[T] >> FooTrait[Int] = Foo[Int]
  def >>(traitRT: TraitRType[?]): RType[?] =
    import scala.quoted.staging.*
    given Compiler = Compiler.make(getClass.getClassLoader)

    val me = this
    val fn = (quotes: Quotes) ?=> {
      import quotes.reflect.*
      val typeParamTypes = reflect.TypeSymbolMapper.deepApply(me, traitRT)(using quotes)
      val classQuotedTypeRepr = TypeRepr.typeConstructorOf(clazz)
      RType.unwindType(quotes)(classQuotedTypeRepr.appliedTo(typeParamTypes))
    }
    withQuotes(fn)

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
    mixins: List[String],
    classType: Option[Type[R]] = None // Internal use only! (fixes broken Classloader for Java classes inside a macro)
) extends ClassRType[R]:

  val typedName: TypedName = name

  lazy val clazz: Class[?] = Class.forName(name)

  override def isAppliedType: Boolean = !typeParamSymbols.isEmpty

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val classType: quoted.Type[R] =
      this.classType.getOrElse(quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]])
    val classTypeRepr = TypeRepr.of[R](using classType)
    val fieldTypes = fields.map { f =>
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(classTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaClassRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("typeParamValues", this.typeParamValues),
        JsonField("fields", this.fields),
        JsonField("annotations", this.annotations),
        JsonField("mixins", this.mixins)
      )
    )
