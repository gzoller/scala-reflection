package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import rtypes.*

object ReflectOnField:
  def apply[Q](quotes: Quotes)(
    fieldType: RType[Q],
    valDef: quotes.reflect.ValDef, 
    index: Int, 
    dad: Option[ClassRType[_]],
    fieldDefaultMethods: Map[Int, (String,String)],
    isNonValConstructorField: Boolean = false
  )(using Type[Q]): FieldInfo = 
    import quotes.reflect.* 

    // Get any field annotations (from body of class--they're not on the constructor fields)
    val fieldAnnos = dad match {
      // case Some(c) if c.isInstanceOf[JavaClassInfo] => Map.empty[String,Map[String,String]]
      case _ =>
        val baseAnnos = dad.flatMap( _.fields.find(_.name == valDef.name) ).map(_.annotations).getOrElse(Map.empty[String,Map[String,String]])
        baseAnnos ++ valDef.symbol.annotations.map{ a =>
          val quotes.reflect.Apply(_, params) = a: @unchecked
          val annoName = a.symbol.signature.resultSig
          (annoName, annoSymToString(quotes)(params))
        }.toMap
    }

    // Figure out the original type symbols, i.e. T, (if any)
    val valTypeRef = valDef.tpt.tpe.asInstanceOf[quotes.reflect.TypeRef]
    val isTypeParam = valTypeRef.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None

    ScalaFieldInfo(
      index, 
      valDef.name, 
      fieldType, 
      fieldAnnos,
      fieldDefaultMethods.get(index),
      originalTypeSymbol,
      isNonValConstructorField
      )

