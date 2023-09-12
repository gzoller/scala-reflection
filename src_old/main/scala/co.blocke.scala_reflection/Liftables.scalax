package co.blocke.scala_reflection

import quoted.*
import java.io.*
import java.util.*
import ToExpr.*

import info._
import impl.SelfRefRType

given ToExpr[TypeSymbol] with {
  def apply(t: TypeSymbol)(using Quotes) = '{ ${Expr(t.asInstanceOf[String])}.asInstanceOf[TypeSymbol] }
}

given ToExpr[RType] with {
  def apply(x: RType)(using Quotes) =
    '{ RType.deserialize(${Expr(x.serialize) }).asInstanceOf[RType] }
}

given ToExpr[TypeMemberInfo] with {
  def apply(x: TypeMemberInfo)(using Quotes) =
    '{ new TypeMemberInfo(${Expr(x.name)}, ${Expr(x.typeSymbol)}, ${ Expr(x.memberType) } ) }
}

given ToExpr[SelfRefRType] with {
  def apply(x: SelfRefRType)(using Quotes) =
    '{ new SelfRefRType(${Expr(x.name)}) }
}
