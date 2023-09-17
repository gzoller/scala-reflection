package co.blocke.scala_reflection
package exprs

import scala.reflect.ClassTag
import scala.quoted.*
import rtypes.*

case class Thing[T](stuff: T)

object Classes:

  def makeExpr[T](crt: ClassRType[T])(using q:Quotes)(using ct: Type[T]): Expr[RType[T]] = 
    import q.reflect.*
    import Liftables.{TypeSymbolToExpr, TypedNameToExpr, ListTypeSymbolToExpr}

    crt match {

      case sc: ScalaClassRType[T] => 

        val typeMembers = Expr.ofList{sc._typeMembers.map( tm =>
          val tt = TypeRepr.of[tm.T].asType.asInstanceOf[Type[tm.T]]
          ExprMaster.makeExpr(tm)(using q)(using tt).asInstanceOf[Expr[TypeMemberRType]]
        )}

        // This silly little piece of drama is sadly necessary to keep Scala's ADHD type-checkker happy.
        // We need to take the incoming RType (z), which has some given type, and explicitly cast it to
        // RType[_] to make Scala happy.  Sigh.  It works great when we do, so...
        inline def stripType( z: Expr[RType[_]])(using q:Quotes): Expr[RType[_]] =
          '{ $z.asInstanceOf[RType[_]] }

        // Created lifted (Expr) list of our class' fields (FieldInfo)
        val caseFields: Expr[List[FieldInfo]] = 
            Expr.ofList{sc._fields.map( f => 
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

        //-------------------------------
        // Recipe:  How to instantiate a parameterized class by applying type first like: case class Foo[T](arg: T)  We apply T to an actual type, then supply the args.
        //-------------------------------
        Apply(
            TypeApply(
                Select.unique(New(TypeTree.of[ScalaClassRType]),"<init>"),
                List(TypeTree.of[T])
            ),
            List(
                Expr(sc.name).asTerm,
                Expr(sc.typedName).asTerm,
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



//===========================================================================================

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


Class.forName() replacement:
I can't reproduce any error with putting Class.forName in a macro. I'm curious what's happening.
Anyway, you probably want either a quoted.Type (companion object method of) or a quoted.quotes.reflect.Symbol (companion object method classSymbol)


// Convert Option[Expr[T]] --> Expr[Option[T]]
def ofOption[T](xs: Option[Expr[T]])(using Type[T])(using Quotes): Expr[Option[T]] =
  if (xs.isEmpty) Expr(None) else '{ Some(${xs.get}) }

*/