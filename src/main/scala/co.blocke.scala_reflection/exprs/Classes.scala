package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

case class Thing[T](stuff: T)

object Classes:

  def makeExpr[T](crt: ClassRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.TypeSymbolToExpr
    import dotty.tools.dotc.core.Contexts.* 

    crt match {

      case sc: ScalaClassRType[T] => 

        println("IN> "+sc.name)
        val typeMembers = Expr.ofList(sc._typeMembers.map{ tm =>
          val typedTm = tm.asInstanceOf[TypeMemberRType[tm.memberType.T]]
          val tt = tm.toType(q).asInstanceOf[Type[tm.memberType.T]]
          ExprMaster.makeExpr(typedTm)(using q)(using tt).asInstanceOf[Expr[TypeMemberRType[_]]]
        })
        val caseFields = Expr.ofList(sc._fields.map{ f =>
          println("   Field "+f)
          val typedF = f.asInstanceOf[ScalaFieldInfo[f.fieldType.T]]
          println("   --1--")
          val tt = f.fieldType.toType(q).asInstanceOf[Type[f.fieldType.T]]
          println("   --2--")
          ExprMaster.makeFieldExpr(typedF)(using q)(using tt).asInstanceOf[Expr[FieldInfo[_]]]
        })

        //-------------------------------
        // Recipe:  How to instantiate a parameterized class by applying type
        //-------------------------------
        Apply(
            TypeApply(
                Select.unique(New(TypeTree.of[ScalaClassRType]),"<init>"),
                List(TypeTree.of[T])
            ),
            List(
                Expr(sc.name).asTerm,
                Expr(sc.paramSymbols).asTerm,
                typeMembers.asTerm,
                caseFields.asTerm,
                Expr(sc._annotations).asTerm,
                Expr(sc.paths).asTerm,
                Expr(sc._mixins).asTerm,
                Expr(sc.isAppliedType).asTerm,
                Expr(sc.isValueClass).asTerm,
                Expr(sc.isCaseClass).asTerm
            )
        ).asExprOf[RType[T]]

        // '{ 
        //   ScalaClassRType[T](
        //     ${Expr(sc.name)}, 
        //     ${Expr(sc.paramSymbols)},
        //     ${typeMembers},
        //     ${caseFields},
        //     ${Expr(sc._annotations)},
        //     ${Expr(sc.paths)},
        //     ${Expr(sc._mixins)},
        //     ${Expr(sc.isAppliedType)}, 
        //     ${Expr(sc.isValueClass)}, 
        //     ${Expr(sc.isCaseClass)}
        //   ) 
        // }

    }


    /*

    : Quotes.this.Nested

Returns a nested quote with this symbol as splice owner (Symbol.spliceOwner).

Changes the owner under which the definition in a quote are created.

Usages:

def rhsExpr(using q: Quotes): Expr[Unit] =
  import q.reflect._
  '{ val y = ???; (y, y) }
def aValDef(using q: Quotes)(owner: q.reflect.Symbol) =
  import q.reflect._
  val sym = Symbol.newVal(owner, "x", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.noSymbol)
  val rhs = rhsExpr(using sym.asQuotes).asTerm
  ValDef(sym, Some(rhs))
//{
def inQuotes(using q: Quotes) = {
  import q.reflect._
//}
  new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term =
      tree match
        case tree: Ident =>
          given Quotes = owner.asQuotes
          // Definitions contained in the quote will be owned by `owner`.
          // No need to use `changeOwner` in this case.
          '{ val x = ???; x }.asTerm
//{
}
//}

    */