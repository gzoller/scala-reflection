package co.blocke.scala_reflection
package exprs

import scala.reflect.ClassTag
import scala.quoted.*
import rtypes.*

case class Thing[T](stuff: T)

object Classes:

  def makeExpr[T](crt: ClassRType[T])(using q: Quotes)(using ct: Type[T]): Expr[RType[T]] =
    import q.reflect.*
    import Liftables.{TypeSymbolToExpr, TypedNameToExpr, ListTypeSymbolToExpr}

    // This silly little piece of drama is sadly necessary to keep Scala's ADHD type-checkker happy.
    // We need to take the incoming RType (z), which has some given type, and explicitly cast it to
    // RType[_] to make Scala happy.  Sigh.  It works great when we do, so...
    inline def stripType(z: Expr[RType[_]])(using q: Quotes): Expr[RType[_]] =
      '{ $z.asInstanceOf[RType[_]] }

    crt match {

      case sc: ScalaClassRType[T] =>
        val typeMembers = Expr.ofList {
          sc.typeMembers.map(tm =>
            val tt = TypeRepr.of[tm.T].asType.asInstanceOf[Type[tm.T]]
            ExprMaster.makeExpr(tm)(using q)(using tt).asInstanceOf[Expr[TypeMemberRType]]
          )
        }

        val typeValues = Expr.ofList {
          sc.typeParamValues.map(tv =>
            val tt = tv.toType(quotes)
            ExprMaster.makeExpr(tv)(using q)(using tt).asInstanceOf[Expr[RType[_]]]
          )
        }

        // Created lifted (Expr) list of our class' fields (FieldInfo)
        val caseFields: Expr[List[FieldInfo]] =
          Expr.ofList {
            sc.fields.map(f =>
              val fieldtt = f.fieldType.toType(quotes)
              val fieldTypeExpr = stripType(ExprMaster.makeExpr(f.fieldType)(using q: Quotes)(using fieldtt))
              Apply(
                Select.unique(New(TypeTree.of[ScalaFieldInfo]), "<init>"),
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
            )
          }

        val nonConstructorFields: Expr[List[NonConstructorFieldInfo]] =
          Expr.ofList {
            sc.nonConstructorFields.map(f =>
              val fieldtt = f.fieldType.toType(quotes)
              val fieldTypeExpr = stripType(ExprMaster.makeExpr(f.fieldType)(using q: Quotes)(using fieldtt))
              Apply(
                Select.unique(New(TypeTree.of[NonConstructorFieldInfo]), "<init>"),
                List(
                  Expr(f.index).asTerm,
                  Expr(f.getterLabel).asTerm, // name
                  Expr(f.getterLabel).asTerm,
                  Expr(f.setterLabel).asTerm,
                  Expr(f.getterIsVal).asTerm,
                  fieldTypeExpr.asTerm,
                  Expr(f.annotations).asTerm,
                  Expr(f.originalSymbol).asTerm
                )
              ).asExprOf[NonConstructorFieldInfo]
            )
          }

        val sealedChildren: Expr[List[RType[_]]] =
          Expr.ofList {
            sc.sealedChildren.map(c =>
              val childtt = c.toType(quotes).asInstanceOf[Type[c.T]]
              stripType(ExprMaster.makeExpr(c)(using q: Quotes)(using childtt))
            )
          }

        // -------------------------------
        // Recipe:  How to instantiate a parameterized class by applying type first like: case class Foo[T](arg: T)  We apply T to an actual type, then supply the args.
        // -------------------------------
        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[ScalaClassRType]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(sc.name).asTerm,
            Expr(sc.typedName).asTerm,
            Expr(sc.typeParamSymbols).asTerm,
            typeValues.asTerm,
            typeMembers.asTerm,
            caseFields.asTerm,
            Expr(sc.annotations).asTerm,
            Expr(sc.mixins).asTerm,
            Expr(sc.isAppliedType).asTerm,
            Expr(sc.isValueClass).asTerm,
            Expr(sc.isCaseClass).asTerm,
            Expr(sc.isAbstractClass).asTerm,
            Expr(sc.typeParamPaths).asTerm,
            nonConstructorFields.asTerm,
            sealedChildren.asTerm
          )
        ).asExprOf[RType[T]]

      case jc: JavaClassRType[T] =>
        // Created lifted (Expr) list of our class' fields (FieldInfo)
        val fields: Expr[List[FieldInfo]] =
          Expr.ofList {
            jc.fields.map(f =>
              val fieldTypeExpr = f.fieldType match {
                case e: JavaEnumRType[?] => e.expr.get // special case: pre-cooked Expr
                case _ =>
                  val fieldtt = f.fieldType.toType(quotes)
                  stripType(ExprMaster.makeExpr(f.fieldType)(using q: Quotes)(using fieldtt))
              }
              Apply(
                Select.unique(New(TypeTree.of[NonConstructorFieldInfo]), "<init>"),
                List(
                  Expr(f.index).asTerm,
                  Expr(f.name).asTerm,
                  Expr(f.asInstanceOf[NonConstructorFieldInfo].getterLabel).asTerm,
                  Expr(f.asInstanceOf[NonConstructorFieldInfo].setterLabel).asTerm,
                  Expr(f.asInstanceOf[NonConstructorFieldInfo].getterIsVal).asTerm,
                  fieldTypeExpr.asTerm,
                  Expr(f.annotations).asTerm,
                  Expr(f.originalSymbol).asTerm
                )
              ).asExprOf[FieldInfo]
            )
          }

        val typeValues = Expr.ofList {
          jc.typeParamValues.map(tv =>
            val tt = tv.toType(quotes)
            ExprMaster.makeExpr(tv)(using q)(using tt).asInstanceOf[Expr[RType[_]]]
          )
        }

        Apply(
          TypeApply(
            Select.unique(New(TypeTree.of[JavaClassRType]), "<init>"),
            List(TypeTree.of[T])
          ),
          List(
            Expr(jc.name).asTerm,
            fields.asTerm,
            Expr(jc.typeParamSymbols).asTerm,
            typeValues.asTerm,
            Expr(jc.annotations).asTerm,
            Expr(jc.mixins).asTerm,
            Expr(None).asTerm
          )
        ).asExprOf[RType[T]]
    }
