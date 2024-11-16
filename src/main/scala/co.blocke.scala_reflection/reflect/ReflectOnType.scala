package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import rtypeRefs.*
import rtypeRefs.{AliasRef, JavaEnumRef, LeftRightRef, PrimitiveRef, ScalaEnumerationRef, SelfRefRef, TypeSymbolRef, UnknownRef}

/** ReflectOnType in the initial point of entry and triage when we get a Type to reflect on.  Most of the time this Type
  * will be some class, but we can't assume that.  Intersection and Union types are not classes and have no classSymbol.
  * Even for "normal" classes we want to collect some initial information before diving into the class itself.  The goal
  * is to smooth out some of the "-isms" and oddments before reflecing on the class, to make the job a little easier.
  */
object ReflectOnType: // extends NonCaseClassReflection:

  // Pre-bake primitive types w/cached builder functions
  private val allBasicTypesMap = PrimitiveRef.primTypeMap ++ TimeRef.simpleTypeMap ++ NetRef.simpleTypeMap

  def apply[T](
      quotes: Quotes
  )(aType: quotes.reflect.TypeRepr, resolveTypeSyms: Boolean = true)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[T] =
    import quotes.reflect.*

    val dealiased = aType.dealias
    val tname = util.TypedName(quotes)(dealiased)
    val z: Boolean = if tname.toString.contains("Any") then true else false
    allBasicTypesMap
      .get(tname)
      .map(fn => fn(quotes))
      .getOrElse {
        val className = dealiased.asInstanceOf[TypeRef] match {
          case AndType(_, _)                               => Clazzes.INTERSECTION_CLASS
          case OrType(_, _)                                => Clazzes.UNION_CLASS
          case _: dotty.tools.dotc.core.Types.WildcardType => Clazzes.ANY_CLASS
          case normal                                      => normal.classSymbol.get.fullName
        }

        className match
          case Clazzes.ANYVAL_CLASS => AnyValRef()(using quotes)(using aType.asType.asInstanceOf[Type[AnyVal]]).asInstanceOf[RTypeRef[T]]
          case Clazzes.ANY_CLASS    => reflectOnType[T](quotes)(aType, tname, resolveTypeSyms)
          case _ if seenBefore.get(tname) == Some(true) =>
            SelfRefRef[T](className, tname)(using quotes)(using aType.asType.asInstanceOf[Type[T]])
          case _ => reflectOnType[T](quotes)(aType, tname, resolveTypeSyms)
      }
      .asInstanceOf[RTypeRef[T]]

  private def reflectOnType[T](
      quotes: Quotes
  )(aType: quotes.reflect.TypeRepr, typedName: TypedName, resolveTypeSyms: Boolean)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[T] =
    import quotes.reflect.*
    implicit val q = quotes

    val typeRef = aType.asInstanceOf[TypeRef]
    typeRef.classSymbol match {

      case None => // Sometimes And/Or types don't have class symbols, so we need to test them again separately...
        typeRef.dealias match {
          // Intersection Type
          // ----------------------------------------
          case AndType(left, right) =>
            left.asType match
              case '[lt] =>
                right.asType match
                  case '[rt] =>
                    val resolvedLeft = reflect.ReflectOnType[lt](quotes)(left, false)
                    val resolvedRight = reflect.ReflectOnType[rt](quotes)(right, false)
                    val typeSymbols = List("L", "R")
                    LeftRightRef[lt & rt](
                      Clazzes.INTERSECTION_CLASS,
                      typeSymbols,
                      resolvedLeft,
                      resolvedRight,
                      LRKind.INTERSECTION
                    )(using quotes)(using Type.of[lt & rt]).asInstanceOf[RTypeRef[T]]

          // Union Type
          // ----------------------------------------
          case OrType(left, right) =>
            left.asType match
              case '[lt] =>
                right.asType match
                  case '[rt] =>
                    val resolvedLeft = reflect.ReflectOnType[lt](quotes)(left, false)
                    val resolvedRight = reflect.ReflectOnType[rt](quotes)(right, false)
                    val typeSymbols = List("L", "R")
                    LeftRightRef[lt | rt](
                      Clazzes.UNION_CLASS,
                      typeSymbols,
                      resolvedLeft,
                      resolvedRight,
                      LRKind.UNION
                    )(using quotes)(using Type.of[lt | rt]).asInstanceOf[RTypeRef[T]]
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

        typeRef.dealias match {
          case AndType(left, right) =>
            left.asType match
              case '[lt] =>
                right.asType match
                  case '[rt] =>
                    val resolvedLeft = reflect.ReflectOnType[lt](quotes)(left, false)
                    val resolvedRight = reflect.ReflectOnType[rt](quotes)(right, false)
                    val typeSymbols = List("L", "R")
                    LeftRightRef[lt & rt](
                      Clazzes.INTERSECTION_CLASS,
                      typeSymbols,
                      resolvedLeft,
                      resolvedRight,
                      LRKind.INTERSECTION
                    )(using quotes)(using Type.of[lt & rt]).asInstanceOf[RTypeRef[T]]

          // Union Type
          // ----------------------------------------
          case OrType(left, right) =>
            left.asType match
              case '[lt] =>
                right.asType match
                  case '[rt] =>
                    val resolvedLeft = reflect.ReflectOnType[lt](quotes)(left, false)
                    val resolvedRight = reflect.ReflectOnType[rt](quotes)(right, false)
                    val typeSymbols = List("L", "R")
                    LeftRightRef[lt | rt](
                      Clazzes.UNION_CLASS,
                      typeSymbols,
                      resolvedLeft,
                      resolvedRight,
                      LRKind.UNION
                    )(using quotes)(using Type.of[lt | rt]).asInstanceOf[RTypeRef[T]]

          // Scala3 opaque type alias
          // ----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType if classSymbol == Symbol.classSymbol(Clazzes.ANY_CLASS) && typeRef.isOpaqueAlias =>
            val translucentSuperType = typeRef.translucentSuperType
            val wrappedType = translucentSuperType.asType match
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(translucentSuperType)
            AliasRef[T](typeRef.show, wrappedType)(using quotes)(using aType.asType.asInstanceOf[Type[T]])

          // Scala3 Tasty-equipped type incl. primitive types
          // Traits and classes w/type parameters are *not* here... they're AppliedTypes
          // ----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType =>
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param) // Is 'T' or a "real" type?  (true if T)
            classSymbol match {

              case cs if isTypeParam =>
                TypeSymbolRef(typeRef.name)(using quotes)(using Type.of[Any]).asInstanceOf[RTypeRef[T]]

              case cs if is2xEnumeration =>
                val enumerationClassSymbol = typeRef.qualifier.termSymbol.moduleClass
                typeRef.asType match
                  case '[y] =>
                    ScalaEnumerationRef[y](
                      className,
                      enumerationClassSymbol.declaredFields.map(_.name)
                    )(using quotes)(using Type.of[y]).asInstanceOf[RTypeRef[T]]

              // Symbol.requiredModule("co.blocke.scalajack.json.run.NonZeroInt").methodMembers
              case a if a == defn.AnyClass =>
                typeRef.asType match
                  case '[y] =>
                    typeRef.typeSymbol.fullName match
                      case "scala.Any" => AnyRef().asInstanceOf[RTypeRef[T]] // Any type
                      case n => // presume NeoType (CAUTION--may not always be true in the future!)
                        NeoTypeRef[y](typeRef.typeSymbol.name, n.replaceAll("""\.\w+\$package\$""", "").asInstanceOf[TypedName]).asInstanceOf[RTypeRef[T]]

              case cs if !isJavaEnum => // Non-parameterized classes
                // This chunk here catches type members who's values are applied types, eg type foo = List[Int]
                typeRef.dealias match
                  case AppliedType(t, tob) =>
                    val foundType: Option[RTypeRef[T]] = ExtractorRegistry.extractors.collectFirst {
                      case e if e.matches(quotes)(classSymbol) =>
                        e.extractInfo[T](quotes)(t, tob, classSymbol)
                    }
                    foundType
                      .getOrElse {
                        // Nope--we've got a parameterized class or trait here
                        ReflectOnClass(quotes)(typeRef, util.TypedName(quotes)(typeRef), resolveTypeSyms, tob)
                      }
                  case _ =>
                    ReflectOnClass(quotes)(typeRef, typedName, resolveTypeSyms)

              case _: Symbol => // Java Enum
                typeRef.asType match
                  case '[y] =>
                    JavaEnumRef(
                      aType.classSymbol.get.fullName,
                      aType.classSymbol.get.children.map(_.name)
                    )(using quotes)(using Type.of[y]).asInstanceOf[RTypeRef[T]]
            }

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
