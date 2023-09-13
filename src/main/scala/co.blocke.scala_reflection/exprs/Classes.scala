package co.blocke.scala_reflection
package exprs

import scala.quoted.*
import rtypes.*

case class Thing[T](stuff: T)

object Classes:

  def makeExpr[T](crt: ClassRType[T])(using q:Quotes)(using Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.{TypeSymbolToExpr,TypedNameToExpr}
    import dotty.tools.dotc.core.Contexts.* 

    crt match {

      case sc: ScalaClassRType[T] => 

        println("IN> "+sc.name)
        val typeMembers = Expr.ofList(sc._typeMembers.map( tm =>
          ExprMaster.makeExpr(tm)(using q)(using RType.quotedTypeCache(tm.typedName).asInstanceOf[Type[tm.T]]).asInstanceOf[Expr[TypeMemberRType]]
        ))
        val caseFields = Expr.ofList(sc._fields.map{ f =>
          println("   Field "+f)
          makeFieldExpr(f)(using q)(using RType.quotedTypeCache(f.fieldType.typedName).asInstanceOf[Type[f.fieldType.T]]).asInstanceOf[Expr[FieldInfo]]
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
                // Expr(sc.typedName).asTerm,
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
    }

  def makeFieldExpr[T]( fieldInfo: FieldInfo )(using q:Quotes)(using tt:Type[T]): Expr[FieldInfo] =
    import q.reflect.*
    import Liftables.OptTypeSymbolToExpr

    println("Making field: "+fieldInfo.name)
    val fi = fieldInfo.asInstanceOf[ScalaFieldInfo]
    // using RType.quotedTypeCache(fieldInfo.fieldType.typedName)
    val fieldTypeExpr = ExprMaster.makeExpr(fieldInfo.fieldType)(using q)(using tt.asInstanceOf[Type[fieldInfo.fieldType.T]]).asInstanceOf[Expr[RType[fieldInfo.fieldType.T]]]
    println("Ready field: "+fieldInfo.name)

    Apply(
      Select.unique(New(TypeTree.of[ScalaFieldInfo]),"<init>"),
      List(
        Expr(fi.index).asTerm, 
        Expr(fi.name).asTerm,
        fieldTypeExpr.asTerm, 
        Expr(fi.annotations).asTerm, 
      )
    ).asExprOf[FieldInfo]
 
    // '{ 
    //   ScalaFieldInfo(
    //     ${Expr(fi.index)}, 
    //     ${Expr(fi.name)},
    //     ${ fieldTypeExpr }, 
    //     ${Expr(fi.annotations)}, 
        // ${Expr(fi.defaultValueAccessorName)}, 
        // ${Expr(fi.originalSymbol)},
        // ${Expr(fi.isNonValConstructorField)}
    //  ).asInstanceOf[FieldInfo]
    // }

//===========================================================================================
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


    /* Discord Wisdom if needed...

    case class Thing[G](stuff: G)

object Macro {
  inline def invokeBroken = ${ implBroken }

  def implBroken(using Quotes) = {
    import quotes.reflect.*
    // In a macro...
    val ast = Apply(
      Select.unique(New(TypeTree.of[Thing[Int]]), "<init>"), // also tried thingClassSym.primaryconstructor instead of "<init>"
      List(Expr(25).asTerm)
    )

    println(ast.show)
    ast.asExprOf[Thing[Int]]
  }

  inline def invokeCorrect = ${ implCorrect }

  def implCorrect(using Quotes) = {
    import quotes.reflect.*

    val destTpe = TypeRepr.of[Thing[Int]]

    val (tpe, constructor, tpeArgs) = { // you should pattern match here, obviously not safe
      val AppliedType(repr, tpes) = destTpe: @unchecked 
      (repr, repr.typeSymbol.primaryConstructor, tpes)
    }

    val ast = New(Inferred(tpe)) // take the actual class tpe (destTpe is the already applied one)
      .select(constructor) // take the actuak class' constructor (not the already applied one!)
      .appliedToTypes(tpeArgs) // apply it to type args
      .appliedToArgs(List(Expr(25).asTerm))


    report.info(ast.show)
    ast.asExprOf[Thing[Int]]
  }
}
*/