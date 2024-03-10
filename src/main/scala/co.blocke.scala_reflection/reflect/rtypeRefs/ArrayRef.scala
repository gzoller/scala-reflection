package co.blocke.scala_reflection
package reflect
package rtypeRefs

import scala.quoted.*
import rtypes.ArrayRType
import util.{JsonField, JsonObjectBuilder}
import Liftables.TypeSymbolToExpr

/** Reference to a Scala Array
  * @param name             simple name of the class
  * @param typeParamSymbols List (of 1) parameter symbole, i.e. "A", as in Array[A]
  * @param elementRef       Ref for the element type of the array, eg. Array[Person]
  */
case class ArrayRef[R](
    name: String,
    typeParamSymbols: List[TypeSymbol],
    elementRef: RTypeRef[?]
)(using quotes: Quotes)(using tt: Type[R])
    extends RTypeRef[R]
    with CollectionRef[R]:
  import quotes.reflect.*
  val refType = tt

  val unitVal = '{ null }.asExprOf[R]

  val isMutable = true

  val expr =
    Apply(
      TypeApply(
        Select.unique(New(TypeTree.of[ArrayRType[R]]), "<init>"),
        List(TypeTree.of[R])
      ),
      List(
        Expr(name).asTerm,
        Expr(typeParamSymbols).asTerm,
        elementRef.expr.asTerm
      )
    ).asExprOf[RType[R]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ArrayRType"),
        JsonField("name", name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("elementType", this.elementRef)
      )
    )

  // override def toType(quotes: Quotes): quoted.Type[R] =
  //   import quotes.reflect.*
  //   val collectionType: quoted.Type[R] =
  //     quotes.reflect.TypeRepr.typeConstructorOf(Class.forName(name)).asType.asInstanceOf[quoted.Type[R]]
  //   val elType = elementRef.toType(quotes)
  //   val collectionTypeRepr = TypeRepr.of[R](using collectionType)
  //   val elTypeRepr = TypeRepr.of[elementRef.T](using elType)
  //   AppliedType(collectionTypeRepr, List(elTypeRepr)).asType.asInstanceOf[quoted.Type[R]]
