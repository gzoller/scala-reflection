package co.blocke.scala_reflection
package rtypes

case class NeoTypeRType[T](
    name: String
) extends RType[T]:

  val typedName: TypedName = name
  override lazy val clazz = classOf[Any]
