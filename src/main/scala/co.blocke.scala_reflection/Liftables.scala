package co.blocke.scala_reflection

import quoted._
import java.io._
import java.util._
import Liftable._

import info._
import impl.SelfRefRType

given Liftable[TypeSymbol] {
  def toExpr(t: TypeSymbol) = '{ ${Expr(t.asInstanceOf[String])}.asInstanceOf[TypeSymbol] }
}

given Liftable[RType] {
  def toExpr(x: RType) =
    '{ RType.deserialize(${Expr(x.serialize) }).asInstanceOf[RType] }
}

given Liftable[TypeMemberInfo] {
  def toExpr(x: TypeMemberInfo) =
    '{ new TypeMemberInfo(${Expr(x.name)}, ${Expr(x.typeSymbol)}, ${ Expr(x.memberType) } ) }
}

given Liftable[SelfRefRType] {
  def toExpr(x: SelfRefRType) =
    '{ new SelfRefRType(${Expr(x.name)}) }
}
