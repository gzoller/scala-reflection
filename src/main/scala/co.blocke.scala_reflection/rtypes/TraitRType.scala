package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes


case class TraitRType[R] (
    name: String, 
    fields: List[FieldInfo],
    actualParameterTypes: List[RType[_]] = List.empty[RType[_]],  //<-- TODO: Not sure this field is needed!
    paramSymbols: List[TypeSymbol] = List.empty[TypeSymbol],
  ) extends RType[R] with AppliedRType: 

  val typedName: TypedName = 
    if actualParameterTypes.nonEmpty then
      name + actualParameterTypes.map(_.typedName).toList.mkString("[",",","]")
    else
      name
  lazy val clazz: Class[_] = Class.forName(name)
 
  override def isAppliedType: Boolean = paramSymbols.nonEmpty

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[_] = this
    // TraitInfo(
    //   name, 
    //   fields.map( _.asInstanceOf[ScalaFieldInfo].resolveTypeParams(paramMap) ),
    //   actualParameterTypes.map( _ match {
    //       case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => paramMap(ts.name.asInstanceOf[TypeSymbol])
    //       case art: AppliedRType if art.isAppliedType => art.resolveTypeParams(paramMap)
    //       case t => t
    //     }),
    //   paramSymbols
    //   )

  // override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
  //   import quotes.reflect.{_, given}
  //   if actualParameterTypes.nonEmpty then
  //     val args = actualParameterTypes.map(_.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type]).toList
  //     implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx 
  //     dotty.tools.dotc.core.Types.AppliedType(
  //       TypeRepr.typeConstructorOf(infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
  //       args
  //       ).asInstanceOf[quotes.reflect.AppliedType]
  //   else
  //     quotes.reflect.TypeRepr.typeConstructorOf(infoClass)

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val traitType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(clazz).asType.asInstanceOf[quoted.Type[R]]
    val traitTypeRepr = TypeRepr.of[R](using traitType)
    val fieldTypes = fields.map{ f => 
      val oneFieldType = f.fieldType.toType(quotes)
      TypeRepr.of[f.fieldType.T](using oneFieldType)
    }
    AppliedType(traitTypeRepr, fieldTypes).asType.asInstanceOf[quoted.Type[R]]

      
  def select(i: Int): RType[_] = 
    if i >= 0 && i <= actualParameterTypes.size-1 then
      actualParameterTypes(i)
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")   


//------------------------------------------------------------

case class SealedTraitRType[R] (
    name: String, 
    children: List[RType[_]]
  ) extends RType[R]:

  val typedName: TypedName = name + children.map(_.typedName).toList.mkString("[",",","]")
  lazy val clazz: Class[_] = Class.forName(name)

  // def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
  //   val newTab = {if suppressIndent then tab else tab+1}
  //   if seenBefore.contains(name) then
  //     {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name) (self-ref recursion)\n"
  //   else
  //     {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName
  //     + s"($name)"
  //     + {if children.isEmpty then "\n" else ":\n"+ tabs(newTab) + "children:\n" + children.map(_.show(newTab+1,name :: seenBefore)).mkString}