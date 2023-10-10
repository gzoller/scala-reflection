package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import rtypeRefs.*

/** ReflectOnType in the initial point of entry and triage when we get a Type to reflect on.  Most of the time this Type
  * will be some class, but we can't assume that.  Intersection and Union types are not classes and have no classSymbol.
  * Even for "normal" classes we want to collect some initial information before diving into the class itself.  The goal
  * is to smooth out some of the "-isms" and oddments before reflecing on the class, to make the job a little easier.
  */
object ReflectOnType: // extends NonCaseClassReflection:

  // Pre-bake primitive types w/cached builder functions
  import rtypeRefs.*
  import Clazzes.*
  private val IntFn = (quotes: Quotes) => PrimitiveRef[Int](INT_CLASS)(using quotes)(using Type.of[Int](using quotes))
  private val LongFn = (quotes: Quotes) => PrimitiveRef[Long](LONG_CLASS)(using quotes)(using Type.of[Long](using quotes))
  private val StringFn = (quotes: Quotes) => PrimitiveRef[String](STRING_CLASS)(using quotes)(using Type.of[String](using quotes))

  private val primFnMap = Map(
    INT_CLASS.asInstanceOf[TypedName] -> IntFn,
    LONG_CLASS.asInstanceOf[TypedName] -> LongFn,
    STRING_CLASS.asInstanceOf[TypedName] -> StringFn
  )

  def apply[T](
      quotes: Quotes
  )(aType: quotes.reflect.TypeRepr, resolveTypeSyms: Boolean = true)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[T] =
    import quotes.reflect.*

    val tname = util.TypedName(quotes)(aType)
    primFnMap
      .get(tname)
      .map(fn => fn(quotes))
      .getOrElse {
        val className = aType.asInstanceOf[TypeRef] match {
          case AndType(_, _)                               => Clazzes.INTERSECTION_CLASS
          case OrType(_, _)                                => Clazzes.UNION_CLASS
          case _: dotty.tools.dotc.core.Types.WildcardType => "scala.Any"
          case normal                                      => normal.classSymbol.get.fullName
        }

        if seenBefore.get(tname) == Some(true) then SelfRefRef[T](className, tname)(using quotes)(using aType.asType.asInstanceOf[Type[T]])
        else
          seenBefore.put(tname, true)
          val result = reflectOnType[T](quotes)(aType, "foom".asInstanceOf[TypedName], resolveTypeSyms)
          seenBefore.put(tname, false)
          result
      }
      .asInstanceOf[RTypeRef[T]]

  private def reflectOnType[T](
      quotes: Quotes
  )(aType: quotes.reflect.TypeRepr, typedName: TypedName, resolveTypeSyms: Boolean)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[T] =
    import quotes.reflect.*

    val typeRef = aType.asInstanceOf[TypeRef]
    typeRef.classSymbol match {

      case None =>
        implicit val q = quotes
        typeRef match {
          // Intersection Type
          // ----------------------------------------
          case AndType(left, right) =>
            val resolvedLeft = left.asType match {
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(left, false)
            }
            val resolvedRight = right.asType match {
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(right, false)
            }
            val typeSymbols = List("L", "R")
            LeftRightRef[IntersectionType](
              Clazzes.INTERSECTION_CLASS,
              typeSymbols,
              resolvedLeft,
              resolvedRight,
              LRKind.INTERSECTION
            )(using quotes)(using Type.of[IntersectionType]).asInstanceOf[RTypeRef[T]]

          // Union Type
          // ----------------------------------------
          case OrType(left, right) =>
            val resolvedLeft = left.asType match {
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(left, false)
            }
            val resolvedRight = right.asType match {
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(right, false)
            }
            val typeSymbols = List("L", "R")
            LeftRightRef[UnionType](
              Clazzes.UNION_CLASS,
              typeSymbols,
              resolvedLeft,
              resolvedRight,
              LRKind.UNION
            )(using quotes)(using Type.of[UnionType]).asInstanceOf[RTypeRef[T]]
        }

      // Most types will have a classSymbol and will be handled here...
      case Some(classSymbol) =>
        // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
        val (is2xEnumeration, isJavaEnum, className) = classSymbol.fullName match {
          case raw if raw == Clazzes.ENUMERATION_CLASS =>
            val enumerationClass = typeRef.typeSymbol.fullName
            if enumerationClass == Clazzes.ENUMERATION_CLASS then
              // If caller did NOT define a type member (type X = Value) inside their Enumeration class
              val enumClassName = typeRef.qualifier
                .asInstanceOf[quotes.reflect.TermRef]
                .termSymbol
                .moduleClass
                .fullName
                .dropRight(1) // chop the '$' off the end!
              (true, false, enumClassName)
            else
              // If caller defined a type member (type X = Value) inside their Enumeration class
              (true, false, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
          case _ if classSymbol.flags.is(Flags.JavaDefined) && classSymbol.flags.is(Flags.Enum) => // trap Java Enum so it doesn't go into ReflectOnClass
            (false, true, classSymbol.fullName)
          case _ =>
            (false, false, classSymbol.fullName)
        }

        typeRef match {
          /*
          // Scala3 opaque type alias
          // ----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType if classSymbol == Symbol.classSymbol("scala.Any") && typeRef.isOpaqueAlias =>
            val translucentSuperType = typeRef.translucentSuperType
            val wrappedType = RType.unwindType(quotes)(translucentSuperType)
            AliasRType(typeRef.show, wrappedType).asInstanceOf[RType[T]]
           */

          // Scala3 Tasty-equipped type incl. primitive types
          // Traits and classes w/type parameters are *not* here... they're AppliedTypes
          // ----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType =>
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param) // Is 'T' or a "real" type?  (true if T)
            classSymbol match {

              /*
              case cs if isTypeParam =>
                TypeSymbolRType(typeRef.name) // TypeSymbols Foo[T] have typeRef of Any

              case cs if is2xEnumeration =>
                val enumerationClassSymbol = typeRef.qualifier.termSymbol.moduleClass
                ScalaEnumerationRType(
                  className,
                  enumerationClassSymbol.declaredFields.map(_.name)
                )

              case a if a == defn.AnyClass =>
                AnyRType().asInstanceOf[RType[T]] // Any type
               */

              case cs if !isJavaEnum => // Non-parameterized classes
                ReflectOnClass(quotes)(typeRef, typedName, resolveTypeSyms)

              /*
              case _: Symbol => // Java Enum
                val enumRT = JavaEnumRType(
                  aType.classSymbol.get.fullName,
                  aType.classSymbol.get.children.map(_.name)
                )
                val fieldTypeExpr = stripType( // pre-cook the Expr while we have all the juicy type information (Java only)
                  exprs.ExprMaster.makeExpr(enumRT.asInstanceOf[RType[T]])(using quotes)(using aType.asType.asInstanceOf[Type[T]])
                )(using quotes)
                enumRT.copy(expr = Some(fieldTypeExpr)).asInstanceOf[RType[T]]
               */
            }

          // Union Type (sometimes it pops up down here for some reason... hmm...)
          // ----------------------------------------
          case OrType(left, right) =>
            implicit val q = quotes
            val resolvedLeft = left.asType match {
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(left, false)
            }
            val resolvedRight = right.asType match {
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(right, false)
            }
            val typeSymbols = List("L", "R")
            LeftRightRef[UnionType](
              Clazzes.UNION_CLASS,
              typeSymbols,
              resolvedLeft,
              resolvedRight,
              LRKind.UNION
            )(using quotes)(using Type.of[UnionType]).asInstanceOf[RTypeRef[T]]

          // Parameterized Types (classes, traits, & collections)
          // ----------------------------------------
          case a @ AppliedType(t, tob) =>
            // First see if we have some sort of collection or other "wrapped" type
            val foundType: Option[RTypeRef[T]] = ExtractorRegistry.extractors.collectFirst {
              case e if e.matches(quotes)(classSymbol) =>
                e.extractInfo[T](quotes)(t, tob, classSymbol)
            }
            foundType
              .getOrElse {
                // Nope--we've got a parameterized class or trait here
                ReflectOnClass(quotes)(a.asInstanceOf[TypeRef], util.TypedName(quotes)(a), resolveTypeSyms, tob)
              }

          case _ => // === No idea!  Unknown entity...
            UnknownRef[T](className)(using quotes)(using aType.asType.asInstanceOf[Type[T]])
        }
    }
