package co.blocke.scala_reflection
package rtypes


/** Arity 1 Collections, e.g. List, Set, Seq */
case class SeqRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _elementType: RType[_],
) extends RType[R] with CollectionRType[R]:

  val typedName: TypedName = name + "[" + _elementType.typedName + "]"
  def selectLimit: Int = 1

  lazy val clazz: Class[_] = Class.forName(name)

  lazy val elementType: RType[_] = _elementType
