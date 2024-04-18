package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.{JavaClassRType, NonConstructorFieldInfo, ScalaClassRType, TypeMemberRType}
import util.{JsonField, JsonObjectBuilder}

trait ClassRef[R] extends RTypeRef[R] with AppliedRef:
  self: RTypeRef[?] =>

  val fields: List[FieldInfoRef]
  val typeParamSymbols: List[TypeSymbol]
  val typeParamValues: List[RTypeRef[?]]
  val annotations: Map[String, Map[String, String]]
  val mixins: List[String]

  val selectLimit: Int = fields.size
  def select(i: Int): RTypeRef[?] =
    if i >= 0 && i < selectLimit then fields(i).fieldRef
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

//------------------------------------------------------------------------------

case class ScalaClassRef[R](
    name: String,
    typedName: TypedName,
    typeParamSymbols: List[TypeSymbol],
    typeParamValues: List[RTypeRef[?]],
    typeMembers: List[TypeMemberRef],
    fields: List[FieldInfoRef],
    annotations: Map[String, Map[String, String]],
    mixins: List[String],
    override val isAppliedType: Boolean,
    isValueClass: Boolean,
    isCaseClass: Boolean,
    isAbstractClass: Boolean,
    nonConstructorFields: List[NonConstructorFieldInfoRef] = Nil, // Populated for non-case classes only
    sealedChildren: List[RTypeRef[?]] = Nil, // Populated only if this is a sealed class or abstract class
    childrenAreObject: Boolean = false,
    typePaths: Map[String, List[List[Int]]]
)(using quotes: Quotes)(using tt: Type[R])
    extends ClassRef[R]
    with Sealable:
  import quotes.reflect.*
  import Liftables.{ListTypeSymbolToExpr, TypedNameToExpr, TypeSymbolToExpr}

  val refType = tt

  val unitVal =
    // If this is a value class we can't assume null (could be an Int wrapped).  So we need to
    // construct one of these beasties with the unit val of the specific wrapped type.  Kinda doesn't matter,
    // but this way should guarantee no class cast exceptions.
    if isValueClass then
      refType match {
        case '[c] =>
          val tpe = TypeRepr.of[c]
          val primaryConstructor = tpe.classSymbol.get.primaryConstructor
          val constructor = Select(New(Inferred(tpe)), primaryConstructor)
          val argss = List(List(fields(0).fieldRef.unitVal.asTerm))
          argss.tail.foldLeft(Apply(constructor, argss.head))((acc, args) => Apply(acc, args)).asExprOf[R]
      }
    else '{ null }.asExprOf[R]

  def isSealed: Boolean = sealedChildren.nonEmpty

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
        Expr(typePaths).asTerm,
        Expr.ofList(nonConstructorFields.map(_.expr.asInstanceOf[Expr[NonConstructorFieldInfo]])).asTerm,
        Expr.ofList(sealedChildren.map(_.expr)).asTerm,
        Expr(childrenAreObject).asTerm
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
        JsonField("sealedChildren", this.sealedChildren),
        JsonField("childrenAreObject", this.childrenAreObject)
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
    typeParamValues: List[RTypeRef[?]],
    annotations: Map[String, Map[String, String]],
    mixins: List[String]
    // classType: Option[Type[_]] = None // Internal use only! (fixes broken Classloader for Java classes inside a macro)
)(using quotes: Quotes)(using tt: Type[R])
    extends ClassRef[R]:
  import quotes.reflect.*
  import Liftables.{ListTypeSymbolToExpr, TypedNameToExpr, TypeSymbolToExpr}

  val typedName: TypedName = name
  val refType = tt

  val unitVal = '{ null }.asExprOf[R]

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
