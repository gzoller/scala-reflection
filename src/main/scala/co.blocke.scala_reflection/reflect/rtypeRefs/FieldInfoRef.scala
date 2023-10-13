package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.{FieldInfo, NonConstructorFieldInfo, ScalaFieldInfo}
import util.{JsonField, JsonObjectBuilder}

/** Base information we keep for all class fields, regardless of whether Scala or Java
  */
trait FieldInfoRef:
  val index: Int
  val name: String
  val fieldRef: RTypeRef[?]
  val originalSymbol: Option[TypeSymbol]
  val annotations: Map[String, Map[String, String]]

  val expr: Expr[FieldInfo]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("name", this.name),
        JsonField("fieldType", this.fieldRef),
        JsonField("originalSymbol", this.originalSymbol),
        JsonField("annotations", this.annotations)
      )
    )

//------------------------------------------------------------

/** Describes reflected information we gleen from a Scala class field
  *
  * @param index
  * @param name
  * @param fieldType
  * @param annotations
  * @param defaultValueAccessorName
  * @param originalSymbol
  * @param isNonValConstructorField
  */
case class ScalaFieldInfoRef(
    index: Int,
    name: String,
    fieldRef: RTypeRef[?],
    annotations: Map[String, Map[String, String]],
    defaultValueAccessorName: Option[(String, String)], // (companion class name, method)
    originalSymbol: Option[TypeSymbol],
    isNonValConstructorField: Boolean = false // meaningful for non-case classes
)(using quotes: Quotes)
    extends FieldInfoRef:
  import quotes.reflect.*
  import Liftables.OptTypeSymbolToExpr

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[ScalaFieldInfo]), "<init>"),
      List(
        Expr(index).asTerm,
        Expr(name).asTerm,
        fieldRef.expr.asTerm,
        Expr(annotations).asTerm,
        Expr(defaultValueAccessorName).asTerm,
        Expr(originalSymbol).asTerm,
        Expr(isNonValConstructorField).asTerm
      )
    ).asExprOf[FieldInfo]

//------------------------------------------------------------

case class NonConstructorFieldInfoRef(
    index: Int,
    name: String,
    getterLabel: String,
    setterLabel: String,
    getterIsVal: Boolean,
    fieldRef: RTypeRef[?],
    annotations: Map[String, Map[String, String]],
    originalSymbol: Option[TypeSymbol] = None
)(using quotes: Quotes)
    extends FieldInfoRef:
  import quotes.reflect.*
  import Liftables.OptTypeSymbolToExpr

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[NonConstructorFieldInfo]), "<init>"),
      List(
        Expr(index).asTerm,
        Expr(name).asTerm,
        Expr(getterLabel).asTerm,
        Expr(setterLabel).asTerm,
        Expr(getterIsVal).asTerm,
        fieldRef.expr.asTerm,
        Expr(annotations).asTerm,
        Expr(originalSymbol).asTerm
      )
    ).asExprOf[FieldInfo]
