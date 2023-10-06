package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes
import reflect.{JsonField, JsonObjectBuilder}

case class TraitRType[R](
    name: String,
    fields: List[FieldInfo],
    typeParamSymbols: List[TypeSymbol] = Nil, // Like T,U
    typeParamValues: List[RType[_]] = Nil // Like Int, Boolean
) extends RType[R]
    with AppliedRType:

  val typedName: TypedName = name
  lazy val clazz: Class[?] = Class.forName(name)

  override def isAppliedType: Boolean = typeParamSymbols.nonEmpty

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val traitType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]]
    val traitTypeRepr = TypeRepr.of[R](using traitType)
    val fieldTypes = fields.map { f =>
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(traitTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]

  def selectLimit: Int = typeParamSymbols.size
  def select(i: Int): RType[?] =
    if i >= 0 && i < selectLimit then typeParamValues(i)
    else throw new ReflectException(s"AppliedType select index $i out of range for $name")

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "TraitRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("typeParamSymbols", this.typeParamSymbols),
        JsonField("typeParamValues", this.typeParamValues)
      )
    )

//------------------------------------------------------------

case class SealedTraitRType[R](
    name: String,
    children: List[RType[_]]
) extends RType[R]:

  val typedName: TypedName = name + children.map(_.typedName).toList.mkString("[", ",", "]")
  lazy val clazz: Class[?] = Class.forName(name)

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "SealedTraitRType"),
        JsonField("name", this.name),
        JsonField("typedName", this.typedName),
        JsonField("children", this.children)
      )
    )
