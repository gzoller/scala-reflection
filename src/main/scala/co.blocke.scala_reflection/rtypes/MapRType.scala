package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes


/** Arity 2 Collections, Map flavors, basiclly */
case class MapRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _elementType: RType[_],  // map key
  _elementType2: RType[_]  // map value
) extends RType[R] with CollectionRType[R]:

  val typedName: TypedName = name + "[" + _elementType.typedName + "," + _elementType2.typedName + "]"
  def selectLimit: Int = 2

  lazy val clazz: Class[_] = Class.forName(name)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val mapType: quoted.Type[R] = super.toType(quotes) 
    val keyParamType: quoted.Type[_elementType.T] = _elementType.toType(quotes)
    val valueParamType: quoted.Type[_elementType2.T] = _elementType2.toType(quotes)
    val mapTypeRepr = TypeRepr.of[R](using mapType)
    val keyParamTypeRepr = TypeRepr.of[_elementType.T](using keyParamType)
    val valueParamTypeRepr = TypeRepr.of[_elementType2.T](using valueParamType)
    AppliedType(mapTypeRepr, List(keyParamTypeRepr, valueParamTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

  override def select(i: Int): RType[_] = 
    i match {
      case 0 => elementType
      case 1 => elementType2
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for ${name}")
    }     
    
  // override def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[_] = this
    // val stage1 = _elementType match {
    //   case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
    //     MapLikeInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]), _elementType2)
    //   case art: AppliedRType if art.isAppliedType => 
    //     MapLikeInfo(name, art.resolveTypeParams(paramMap), _elementType2)
    //   case _ => this
    // }
    // _elementType2 match {
    //   case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
    //     MapLikeInfo(name, stage1._elementType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
    //   case art: AppliedRType if art.isAppliedType => 
    //     MapLikeInfo(name, stage1._elementType, art.resolveTypeParams(paramMap))
    //   case _ => stage1
    // }

  lazy val elementType: RType[_] = _elementType match {
    // case e: SelfRefRType => e.resolve
    case e => e
  }
  lazy val elementType2: RType[_] = _elementType2 match {
    // case e: SelfRefRType => e.resolve
    case e => e
  }
