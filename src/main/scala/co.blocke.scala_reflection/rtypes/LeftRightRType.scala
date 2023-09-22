package co.blocke.scala_reflection
package rtypes

import scala.quoted.Quotes

/** Marker trait for all Scala/Java left/right types (either, intersection, union) */
trait LeftRightRType[R] extends AppliedRType:
  self: RType[R] =>

  lazy val leftType: RType[_]
  lazy val rightType: RType[_]

  val _leftType: RType[_]
  val _rightType: RType[_]

  def _copy( left: RType[_], right: RType[_] ): RType[_]

  override def toType(quotes: Quotes): quoted.Type[R] =
    import quotes.reflect.*
    val lrType: quoted.Type[R] = quotes.reflect.TypeRepr.typeConstructorOf(self.clazz).asType.asInstanceOf[quoted.Type[R]]
    val leftParamType: quoted.Type[_leftType.T] = _leftType.toType(quotes)
    val rightParamType: quoted.Type[_rightType.T] = _rightType.toType(quotes)
    val lrTypeRepr = TypeRepr.of[R](using lrType)
    val leftParamTypeRepr = TypeRepr.of[_leftType.T](using leftParamType)
    val rightParamTypeRepr = TypeRepr.of[_rightType.T](using rightParamType)
    AppliedType(lrTypeRepr, List(leftParamTypeRepr, rightParamTypeRepr)).asType.asInstanceOf[quoted.Type[R]]

  override def isAppliedType: Boolean = 
    (leftType, rightType) match {
      case (artL: AppliedRType, _) if artL.isAppliedType => true
      case (_, artR: AppliedRType) if artR.isAppliedType => true
      case _ => false
    }
  def selectLimit: Int = 2
  

  // TODO!
  override def resolveTypeParams( paramMap: Map[TypeSymbol, RType[_]] ): RType[R] = ???
  //   val stage1 = leftType match {
  //     case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => _copy(paramMap(ts.name.asInstanceOf[TypeSymbol]), rightType)
  //     case art: AppliedRType if art.isAppliedType => _copy(art.resolveTypeParams(paramMap), rightType)
  //     case _ => this
  //   }
  //   rightType match {
  //     case ts: TypeSymbolInfo if paramMap.contains(ts.name.asInstanceOf[TypeSymbol]) => _copy(stage1.asInstanceOf[LeftRightRType].leftType, paramMap(ts.name.asInstanceOf[TypeSymbol]))
  //     case art: AppliedRType if art.isAppliedType => _copy(stage1.asInstanceOf[LeftRightRType].leftType, art.resolveTypeParams(paramMap))
  //     case _ => stage1
  //   }

  def select(i: Int): RType[_] = 
    i match {
      case 0 => leftType
      case 1 => rightType
      case _ => throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")
    }   


  // def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = 
  //   val newTab = {if suppressIndent then tab else tab+1}
  //   val simpleName = this.getClass.getSimpleName match {
  //     case "EitherInfo"       => "Either"
  //     case "UnionInfo"        => "Union"
  //     case "IntersectionInfo" => "Intersection"
  //   }
  //   {if(!suppressIndent) tabs(tab) else ""} + simpleName+":\n"
  //   + tabs(newTab)+ "left--" + leftType.show(newTab+1,name :: seenBefore,true)
  //   + tabs(newTab)+ "right--" + rightType.show(newTab+1,name :: seenBefore,true)  