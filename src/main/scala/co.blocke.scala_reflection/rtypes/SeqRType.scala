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
      
  // override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType =
  //   _elementType match {
  //     case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
  //       SeqLikeInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
  //     case art: AppliedRType if art.isAppliedType => 
  //       SeqLikeInfo(name, art.resolveTypeParams(paramMap))
  //     case _ => this
  //   }

  lazy val elementType: RType[_] = _elementType match {
    // case e: SelfRefRType => e.resolve
    case e => e
  }
