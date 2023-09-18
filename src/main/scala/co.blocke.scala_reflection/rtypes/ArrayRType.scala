package co.blocke.scala_reflection
package rtypes


case class ArrayRType[R] (
  name: String,
  _elementType: RType[_]
) extends RType[R] with CollectionRType[R]:

  val typedName = name + "[" + _elementType.typedName + "]"
  lazy val clazz: Class[_] = Class.forName(name)
      
  // override def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[_] = _elementType
    // _elementType match {
    //   case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
    //     ArrayInfo(name, paramMap(ts.name.asInstanceOf[TypeSymbol]))
    //   case art: AppliedRType if art.isAppliedType => 
    //     ArrayInfo(name, art.resolveTypeParams(paramMap))
    //   case _ => this
    // }

  lazy val elementType: RType[_] = _elementType match {
    // case e: SelfRefRType => e.resolve
    case e => e
  }

  // override def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = 
  //   val newTab = {if suppressIndent then tab else tab+1}
  //   {if(!suppressIndent) tabs(tab) else ""} + s"array of " + elementType.show(newTab,name :: seenBefore,true)
  