package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

case class TraitRType[R](
    name: String,
    typedName: TypedName,
    fields: List[FieldInfo],
    typeParamSymbols: List[TypeSymbol] = Nil, // Like T,U
    typeParamValues: List[RType[?]] = Nil, // Like Int, Boolean
    sealedChildren: List[RType[?]] = Nil, // Populated only if this is a sealed class or abstract class
    childrenAreObject: Boolean = false
) extends RType[R]
    with AppliedRType:

  def isSealed: Boolean = sealedChildren.nonEmpty

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val traitType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]]
    val traitTypeRepr = TypeRepr.of[R](using traitType)
    val fieldTypes = fields.map { f =>
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(traitTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]
