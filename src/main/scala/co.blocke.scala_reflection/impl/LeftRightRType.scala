package co.blocke.scala_reflection
package impl

import info._
import scala.quoted.Quotes

/** Marker trait for all Scala/Java left/right types (either, intersection, union) */
trait LeftRightRType extends AppliedRType:
  self: RType =>

  lazy val leftType: RType
  lazy val rightType: RType

  def _copy( left: RType, right: RType ): RType

  override def toType(quotes: Quotes): quotes.reflect.TypeRepr = 
    import quotes.reflect.{_, given}
    implicit val stuff: dotty.tools.dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx 
    dotty.tools.dotc.core.Types.AppliedType(
      TypeRepr.typeConstructorOf(self.infoClass).asInstanceOf[dotty.tools.dotc.core.Types.Type], 
      List(leftType.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type], rightType.toType(quotes).asInstanceOf[dotty.tools.dotc.core.Types.Type])
      ).asInstanceOf[quotes.reflect.AppliedType]

  override def isAppliedType: Boolean = 
    (leftType match {
      case artL: AppliedRType if artL.isAppliedType => true
      case _ => false
    }) | (rightType match {
      case artR: AppliedRType if artR.isAppliedType => true
      case _ => false
    })

  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType] ): RType = 
    val stage1 = leftType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => _copy(paramMap(ts.name.asInstanceOf[TypeSymbol]), rightType)
      case art: AppliedRType if art.isAppliedType => _copy(art.resolveTypeParams(paramMap), rightType)
      case _ => this
    }
    rightType match {
      case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => _copy(stage1.asInstanceOf[LeftRightRType].leftType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
      case art: AppliedRType if art.isAppliedType => _copy(stage1.asInstanceOf[LeftRightRType].leftType, art.resolveTypeParams(paramMap))
      case _ => stage1
    }

  def select(i: Int): RType = 
    i match {
      case 0 => leftType
      case 1 => rightType
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")
    }   


  def show(tab: Int = 0, seenBefore: List[String] = Nil, supressIndent: Boolean = false, modified: Boolean = false): String = 
    val newTab = {if supressIndent then tab else tab+1}
    val simpleName = this.getClass.getSimpleName match {
      case "EitherInfo"       => "Either"
      case "UnionInfo"        => "Union"
      case "IntersectionInfo" => "Intersection"
    }
    {if(!supressIndent) tabs(tab) else ""} + simpleName+":\n"
    + tabs(newTab)+ "left--" + leftType.show(newTab+1,name :: seenBefore,true)
    + tabs(newTab)+ "right--" + rightType.show(newTab+1,name :: seenBefore,true)  