package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

object Trait:

  def makeExpr[T](aTrait: RType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.{TypeSymbolToExpr, TypedNameToExpr, ListTypeSymbolToExpr}

    inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
        '{ $z.asInstanceOf[RType[_]] }

    aTrait match {

        case scalaTrait: TraitRType[T] => 

            val liftedFields: Expr[List[FieldInfo]] = 
                Expr.ofList{scalaTrait.fields.map( f => 
                    val fieldtt = f.fieldType.toType(quotes)
                    val fieldTypeExpr = stripType( ExprMaster.makeExpr(f.fieldType)(using q:Quotes)(using fieldtt) )
                    Apply(
                    Select.unique(New(TypeTree.of[ScalaFieldInfo]),"<init>"),
                    List(
                        Expr(f.index).asTerm, 
                        Expr(f.name).asTerm,
                        fieldTypeExpr.asTerm, 
                        Expr(f.annotations).asTerm,
                        Expr(f.asInstanceOf[ScalaFieldInfo].defaultValueAccessorName).asTerm,
                        Expr(f.originalSymbol).asTerm,
                        Expr(f.asInstanceOf[ScalaFieldInfo].isNonValConstructorField).asTerm
                    )
                    ).asExprOf[FieldInfo]              
                )}

            Apply(
                TypeApply(
                    Select.unique(New(TypeTree.of[TraitRType[T]]),"<init>"), 
                    List(TypeTree.of[T])
                ),
                List(
                    Expr(scalaTrait.name).asTerm,
                    liftedFields.asTerm,
                    Expr(scalaTrait.typeParamSymbols).asTerm
                )
            ).asExprOf[RType[T]]

        case sealedTrait: SealedTraitRType[T] =>
            val childrenTypes = sealedTrait.children.map( _.toType(quotes) )
            val childrenTypeExpr = sealedTrait.children.zip(childrenTypes).map{ case(rt, rtType) =>
                stripType(
                    ExprMaster.makeExpr(rt)(using q)(using rtType.asInstanceOf[Type[rt.T]]).asInstanceOf[Expr[RType[rt.T]]]
                )
            }
            Apply(
                TypeApply(
                    Select.unique(New(TypeTree.of[SealedTraitRType[T]]),"<init>"), 
                    List(TypeTree.of[T])
                ),
                List(
                    Expr(sealedTrait.name).asTerm,
                    Expr.ofList(childrenTypeExpr).asTerm
                )
            ).asExprOf[RType[T]]
    }
