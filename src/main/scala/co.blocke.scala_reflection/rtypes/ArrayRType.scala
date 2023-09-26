package co.blocke.scala_reflection
package rtypes


case class ArrayRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _elementType: RType[_]
) extends RType[R] with CollectionRType[R]:

  val typedName = name + "[" + _elementType.typedName + "]"
  def selectLimit: Int = 1

  lazy val clazz: Class[_] = Class.forName(name)

  lazy val elementType: RType[_] = _elementType 
