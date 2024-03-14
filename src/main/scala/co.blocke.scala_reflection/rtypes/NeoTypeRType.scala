package co.blocke.scala_reflection
package rtypes

case class NeoTypeRType[T](
    name: String,
    typedName: TypedName
) extends RType[T]:

  override lazy val clazz = classOf[Any]
