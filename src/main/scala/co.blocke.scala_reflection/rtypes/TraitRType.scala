package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes


case class TraitRType[R] (
    name: String, 
    fields: List[FieldInfo],
    typeParamSymbols: List[TypeSymbol] = List.empty[TypeSymbol],
  ) extends RType[R] with AppliedRType: 

  val typedName: TypedName = name
  lazy val clazz: Class[_] = Class.forName(name)
 
  override def isAppliedType: Boolean = typeParamSymbols.nonEmpty
  def selectLimit: Int = fields.size

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
    if i >= 0 && i < fields.size then
      fields(i).fieldType
    else
      throw new ReflectException(s"AppliedType select index $i out of range for ${name}")   


//------------------------------------------------------------

case class SealedTraitRType[R] (
    name: String, 
    children: List[RType[_]]
  ) extends RType[R]:

  val typedName: TypedName = name + children.map(_.typedName).toList.mkString("[",",","]")
  lazy val clazz: Class[_] = Class.forName(name)
