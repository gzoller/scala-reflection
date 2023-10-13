package co.blocke.scala_reflection
package rtypes

case class AliasRType[T](
    definedType: String,
    unwrappedType: RType[?] // Aliases with a parameterized wrapped type are not currently supported, so ConcreteType here.
) extends RType[T]:

  val name: String = definedType.drop(definedType.lastIndexOf('.') + 1)
  val typedName: TypedName = name
  override lazy val clazz = unwrappedType.clazz
