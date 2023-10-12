package co.blocke.scala_reflection
package rtypeRefs

import scala.quoted.*
import rtypes.{JavaClassRType, NonConstructorFieldInfo, ScalaClassRType, TypeMemberRType}
import util.{JsonField, JsonObjectBuilder}

trait ClassRef[R] extends RTypeRef[R] with AppliedRef:
  val name: String
  val fields: List[FieldInfoRef]
  val typeParamSymbols: List[TypeSymbol]
  val typeParamValues: List[RTypeRef[_]]
  val annotations: Map[String, Map[String, String]]
  val mixins: List[String]

  def selectLimit: Int = typeParamSymbols.size
  def select(i: Int): RTypeRef[?] =
    if i >= 0 && i < selectLimit then typeParamValues(i)
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

//------------------------------------------------------------------------------

case class ScalaClassRef[R](
    name: String,
    typedName: TypedName,
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RTypeRef[_]],
    typeMembers: List[TypeMemberRef],
    fields: List[FieldInfoRef],
    annotations: Map[String, Map[String, String]],
    mixins: List[String],
    override val isAppliedType: Boolean,
    isValueClass: Boolean,
    isCaseClass: Boolean,
    isAbstractClass: Boolean,
    typeParamPaths: Map[String, List[List[Int]]] = Map.empty[String, List[List[Int]]], // Trait/Class name -> List of Int (path) for each type param
    nonConstructorFields: List[NonConstructorFieldInfoRef] = Nil, // Populated for non-case classes only
    sealedChildren: List[RTypeRef[_]] = Nil // Populated only if this is a sealed class or abstract class
)(using quotes: Quotes)(using tt: Type[R])
    extends ClassRef[R]:
  import quotes.reflect.*
  import Liftables.{ListTypeSymbolToExpr, TypedNameToExpr, TypeSymbolToExpr}

  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[ScalaClassRType]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typedName).asTerm,
        Expr(typeParamSymbols).asTerm,
        Expr.ofList(typeParamValues.map(_.expr)).asTerm,
        Expr.ofList(typeMembers.map(_.expr.asInstanceOf[Expr[TypeMemberRType]])).asTerm,
        Expr.ofList(fields.map(_.expr)).asTerm,
        Expr(annotations).asTerm,
        Expr(mixins).asTerm,
        Expr(isAppliedType).asTerm,
        Expr(isValueClass).asTerm,
        Expr(isCaseClass).asTerm,
        Expr(isAbstractClass).asTerm,
        Expr.ofList(nonConstructorFields.map(_.expr.asInstanceOf[Expr[NonConstructorFieldInfo]])).asTerm,
        Expr.ofList(sealedChildren.map(_.expr)).asTerm
      )
    ).asExprOf[RType[R]]

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

//------------------------------------------------------------------------------

/** Java class reflection has a special problem... we need the class file, which isn't available during compilation (i.e. inside a macro).
  *  So we need an internal-use-only field (classType) where we store Type[T] for the Java class--which we know during reflection.
  */

case class JavaClassRef[R](
    name: String,
    fields: List[FieldInfoRef],
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RTypeRef[_]],
    annotations: Map[String, Map[String, String]],
    mixins: List[String]
    // classType: Option[Type[_]] = None // Internal use only! (fixes broken Classloader for Java classes inside a macro)
)(using quotes: Quotes)(using tt: Type[R])
    extends ClassRef[R]:
  import quotes.reflect.*
  import Liftables.{ListTypeSymbolToExpr, TypedNameToExpr, TypeSymbolToExpr}

  val typedName: TypedName = name
  val refType = tt

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[JavaClassRType]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr.ofList(fields.map(_.expr)).asTerm,
        Expr(typeParamSymbols).asTerm,
        Expr.ofList(typeParamValues.map(_.expr)).asTerm,
        Expr(annotations).asTerm,
        Expr(mixins).asTerm
      )
    ).asExprOf[RType[R]]

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
