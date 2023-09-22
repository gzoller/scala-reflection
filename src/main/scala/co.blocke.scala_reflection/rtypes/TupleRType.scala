package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes


case class TupleRType[R] (
  name: String,
  typeParamSymbols: List[TypeSymbol],
  _tupleTypes: List[RType[_]]
) extends RType[R] with AppliedRType:

  val typedName: TypedName = name + _tupleTypes.map(_.typedName).toList.mkString("[",",","]")
  def selectLimit: Int = _tupleTypes.size

  lazy val clazz: Class[_] = Class.forName(name)

  // Elements may be self-referencing, so we need to unwind this...
  lazy val tupleTypes = _tupleTypes.map( _ match {
    // case s: SelfRefRType => s.resolve
    case s => s
  })

  override def isAppliedType: Boolean = 
    _tupleTypes.map{ _ match {
      case artL: AppliedRType if artL.isAppliedType => true
      case _ => false
      }}.foldLeft(false)(_ | _)

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[_] = this
    // var needsCopy = false
    // val resolvedTupleTypes = _tupleTypes.map( one => one match {
    //     case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => 
    //       needsCopy = true
    //       paramMap(ts.name.asInstanceOf[TypeSymbol])
    //     case art: AppliedRType if art.isAppliedType => 
    //       needsCopy = true
    //       art.resolveTypeParams(paramMap)
    //     case t => t
    //   }
    // )
    // if needsCopy then
    //   TupleInfo(name, resolvedTupleTypes)
    // else
    //   this
    
  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val tupleType: quoted.Type[R] = super.toType(quotes)
    val tupleElementTypes: List[quoted.Type[_]] = _tupleTypes.map( tt => tt.toType(quotes) )
    val tupleTypeRepr = TypeRepr.of[R](using tupleType)
    val tupleElementTypeRepr = _tupleTypes.zip(tupleElementTypes).map{ case (rt,rtType) =>
      TypeRepr.of[rt.T](using rtType.asInstanceOf[quoted.Type[rt.T]])
    }
    AppliedType(tupleTypeRepr, tupleElementTypeRepr).asType.asInstanceOf[quoted.Type[R]]
    
  def select(i: Int): RType[_] = 
    if i >= 0 && i <= _tupleTypes.size-1 then
      _tupleTypes(i)
    else 
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")

  // def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
  //   val newTab = {if suppressIndent then tab else tab+1}
  //   {if(!suppressIndent) tabs(tab) else ""} + s"""(\n${tupleTypes.map(_.show(newTab,name :: seenBefore)).mkString}""" + tabs(tab) + ")\n"
