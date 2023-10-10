package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import scala.util.matching.Regex
import rtypeRefs.*

/** This is the workhorse of Scala reflection.  Dive into all the Scala-internal bits in the reflection API, extract the
  * juicy bits, and construct our (hopefully) simpler RTypes.
  */
object ReflectOnClass:

  def apply[T](quotes: Quotes)(
      typeRef: quotes.reflect.TypeRef,
      typedName: TypedName,
      resolveTypeSyms: Boolean,
      appliedTob: List[quotes.reflect.TypeRepr] = Nil
  )(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[T] =
    import quotes.reflect.*

    // ===
    // ===  Preparation
    // ===

    // Let's get the className, symbol, and any typeSymbols (eg Foo[T,U]) that may exist.
    // The .replace here "fixes" wrong class name in the case where a class is defined inside an object
    val className = typeRef.classSymbol.get.fullName.replace("$.", "$")
    val symbol = typeRef.classSymbol.get
    val (typeSymbols, typeSymbolValues) = symbol.primaryConstructor.paramSymss match {
      case List(paramSyms: List[Symbol], _) =>
        (
          paramSyms.map(_.name.asInstanceOf[TypeSymbol]),
          typeRef.typeArgs.map { argType =>
            implicit val q = quotes
            argType.asType match
              case '[t] =>
                reflect.ReflectOnType[t](quotes)(argType)
          }
        )
      case _ => (Nil, Nil)
    }

    // Something to allow us to pattern match and extract on default value methods in companion object...
    object DefaultMethod {
      val reg: Regex = """\$lessinit\$greater\$default\$(\d+)""".r
      def unapply(s: quotes.reflect.Symbol): Option[Int] = reg.findFirstIn(s.toString) match {
        case Some(reg(a)) => Some(a.toInt)
        case _            => None
      }
    }

    // Class annotations -> annotation map
    val annoSymbol =
      symbol.annotations.filter(a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
    val classAnnos = annoSymbol.map { a =>
      val quotes.reflect.Apply(_, params) = a: @unchecked
      val annoName = a.symbol.signature.resultSig
      (annoName, annoSymToString(quotes)(params))
    }.toMap

    // Is this class annotated to skip reflection?   If so, return UnknownInfo
    if classAnnos.contains("co.blocke.scala_reflection.Ignore") then UnknownRef[T](symbol.fullName)(using quotes)(using typeRef.asType.asInstanceOf[Type[T]])
    else

      // ===
      // ===  Class Reflection begins...
      // ===

      val isAbstract = symbol.flags.is(quotes.reflect.Flags.Abstract)

      // sealed children, if any
      val isSealed = symbol.flags.is(quotes.reflect.Flags.Sealed)
      val sealedChildrenRTypes = Nil
      /*
        if isSealed then
          symbol.children.map { c =>
            c.tree match {
              case vd: ValDef =>
                RType.unwindType(quotes)(vd.tpt.tpe)
                ObjectRType(vd.symbol.fullName) // sealed object implementation
              case _ => // sealed case class implementation
                val typeDef: dotty.tools.dotc.ast.Trees.TypeDef[?] =
                  c.tree.asInstanceOf[dotty.tools.dotc.ast.Trees.TypeDef[_]]
                RType.unwindType(quotes)(typeDef.typeOpt.asInstanceOf[quotes.reflect.TypeRepr])
            }
          }
        else Nil
       */

      // === Scala 2 Classes ===
      /*
      if symbol.flags.is(quotes.reflect.Flags.Scala2x) then
        Scala2RType(symbol.fullName)

      else if symbol.flags.is(quotes.reflect.Flags.Trait) then
       */
      if symbol.flags.is(quotes.reflect.Flags.Trait) then

        if isSealed then

          // ===
          // ===  Sealed Traits
          // ===
          SealedTraitRef(className, sealedChildrenRTypes)(using quotes)(using typeRef.asType.asInstanceOf[Type[T]])
        else

          // ===
          // ===  Traits (normal, non-sealed)
          // ===
          typeRef match {
            case AppliedType(t, tob) => // parameterized trait
              implicit val q = quotes
              val actualParamTypes = tob.map { oneTob =>
                scala.util
                  .Try {
                    if resolveTypeSyms then
                      oneTob.asType match
                        case '[t] =>
                          reflect.ReflectOnType[t](quotes)(oneTob)
                    else if oneTob.asInstanceOf[quotes.reflect.TypeRef].typeSymbol.flags.is(Flags.Param) then TypeSymbolRef(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)(using quotes)
                    else
                      oneTob.asType match
                        case '[t] =>
                          reflect.ReflectOnType[t](quotes)(oneTob, false)
                  }
                  .toOption
                  .getOrElse {
                    TypeSymbolRef(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)(using quotes)
                  }
              }
              val paramMap: Map[TypeSymbol, RTypeRef[_]] = typeSymbols.zip(actualParamTypes).toMap

              val traitFields = symbol.declaredFields.zipWithIndex.map { (f, index) =>
                val fieldType =
                  // A lot of complex messiness to sew down the mapped type:  Foo[T]( val x: List[T]) where we call Foo[W] from a higher class,
                  // so T -> W.  We need resolveTypeParams to sew down for deeper nested types like List.  Ugh.
                  scala.util
                    .Try {
                      if resolveTypeSyms then
                        typeRef.memberType(f).asType match
                          case '[t] =>
                            reflect.ReflectOnType[t](quotes)(typeRef.memberType(f))
                      else
                        f.tree match {
                          case vd: ValDef if vd.tpt.tpe.typeSymbol.flags.is(Flags.Param) =>
                            paramMap.getOrElse(
                              vd.tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol],
                              vd.tpt.tpe.asType match
                                case '[t] =>
                                  reflect.ReflectOnType[t](quotes)(vd.tpt.tpe)
                            )
                          case _ =>
                            typeRef.memberType(f).asType match
                              case '[t] =>
                                reflect.ReflectOnType[t](quotes)(typeRef.memberType(f), false)
                        }
                    }
                    .toOption
                    .getOrElse {
                      paramMap.getOrElse(
                        f.tree.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol],
                        f.tree.asInstanceOf[ValDef].tpt.tpe.asType match
                          case '[t] =>
                            reflect.ReflectOnType[t](quotes)(f.tree.asInstanceOf[ValDef].tpt.tpe)
                      )
                    }
                val typeSym =
                  f.tree.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol] match {
                    case ts if typeSymbols.contains(ts) => Some(ts)
                    case _                              => None
                  }
                ScalaFieldInfoRef(
                  index,
                  f.name,
                  fieldType,
                  Map.empty[String, Map[String, String]],
                  None,
                  typeSym,
                  true
                )(using quotes)
              }
              TraitRef[T](
                className,
                util.TypedName(quotes)(typeRef),
                traitFields,
                typeSymbols,
                typeSymbolValues
              )(using quotes)(using typeRef.asType.asInstanceOf[Type[T]])

            case _ =>
              implicit val q = quotes

              // non-parameterized trait
              val traitFields = symbol.tree.asInstanceOf[ClassDef].body.collect { case valDef: ValDef =>
                val fieldType =
                  typeRef.memberType(valDef.symbol).asType match
                    case '[t] =>
                      reflect.ReflectOnType[t](quotes)(typeRef.memberType(valDef.symbol))
                ScalaFieldInfoRef(
                  -1,
                  valDef.name,
                  fieldType,
                  Map.empty[String, Map[String, String]],
                  None,
                  None,
                  true
                )(using quotes)
              }
              TraitRef[T](className, util.TypedName(quotes)(typeRef), traitFields)(using quotes)(using typeRef.asType.asInstanceOf[Type[T]])
          }

          /*
      // === Java Class ===
      // User-written Java classes will have the source file.  Java library files will have <no file> for source
      else if symbol.flags.is(Flags.JavaDefined) then
        val nonConstructorFields: List[NonConstructorFieldInfo] =
          ReflectOnField.javaFields(quotes)(symbol, typeRef.asInstanceOf[TypeRepr])
        JavaClassRType(
          symbol.fullName,
          nonConstructorFields,
          typeSymbols,
          typeSymbolValues,
          classAnnos,
          symbol.tree.asInstanceOf[ClassDef].parents.map(_.asInstanceOf[TypeTree].tpe.classSymbol.get.fullName),
          Some(typeRef.asType.asInstanceOf[Type[T]])
        )

      // ===
      // ===  Enums
      // ===
      else if symbol.flags.is(quotes.reflect.Flags.Enum)
      then // Found top-level enum (i.e. not part of a class), e.g. member of a collection
        ScalaEnumRType(symbol.fullName, typeRef.classSymbol.get.children.map(_.name))
           */

      // ===
      // ===  Scala 3 Classes
      // ===
      else if symbol.isClassDef then
        val classDef = symbol.tree.asInstanceOf[ClassDef]

        val isValueClass = typeRef.baseClasses.contains(Symbol.classSymbol("scala.AnyVal"))

        // Get superclasses
        val dad = classDef.parents.headOption match {
          case Some(tt: TypeTree) if !isValueClass && tt.tpe.classSymbol.get.fullName != "java.lang.Object" =>
            ReflectOnClass(quotes)(
              tt.tpe.asInstanceOf[TypeRef],
              util.TypedName(quotes)(tt.tpe),
              resolveTypeSyms
            ) match {
              case ci: ClassRef[?] => Some(ci) // Any kind of class
              case _               => None // e.g. Unknown
            }
          case _ => None
        }

        // Get any case field default value accessor method names (map by field index)
        // Question: What does it take to get Expr[Any]?  If doable, we can go ahead and get the default value right here and poke into the FieldInfo!
        val fieldDefaultMethods = symbol.companionClass match {
          case dotty.tools.dotc.core.Symbols.NoSymbol => Map.empty[Int, (String, String)]
          case s: Symbol =>
            symbol.companionClass.declaredMethods.collect { case DefaultMethod(defaultIndex) =>
              defaultIndex - 1 -> (className + "$", ("$lessinit$greater$default$" + defaultIndex))
            }.toMap
        }

        // All this mucking around in the constructor.... why not just get the case fields from the symbol?
        // Because symbol's case fields lose the annotations!  Pulling from contstructor ensures they are retained.
        val tob = typeRef match {
          case AppliedType(t, tob) => tob
          case _                   => Nil
        }
        implicit val q = quotes
        val typeMembers = classDef.body.collect {
          case TypeDef(typeName, typeTree) if typeSymbols.contains(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name) =>
            val pos = typeSymbols.indexOf(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name)
            tob(pos).asType match
              case '[t] =>
                TypeMemberRef(
                  typeSymbols(pos).toString,
                  reflect.ReflectOnType[t](quotes)(tob(pos).asInstanceOf[TypeRepr])
                )(using quotes)(using Type.of[Any](using quotes))

          case TypeDef(typeName, typeTree) if !typeSymbols.contains(typeName) => // type memebers not used in the constructor
            val tt = typeTree.asInstanceOf[TypeTree].tpe
            tt.asType match
              case '[t] =>
                TypeMemberRef(
                  typeName,
                  reflect.ReflectOnType[t](quotes)(tt)
                )(using quotes)(using Type.of[Any](using quotes))
        }

        // compute type param paths for parameterized class
        val typeParamPaths =
          if typeSymbols.isEmpty then Map.empty[String, List[List[Int]]]
          else TypeSymbolMapper.mapTypeSymbolsForClass(quotes)(classDef, typeSymbols)

        val constructorParams = // Annoying "ism"... different param order dep on whether class is parameterized or not!
          if classDef.constructor.paramss.tail == Nil then classDef.constructor.paramss.head.params
          else classDef.constructor.paramss.tail.head.params

        if symbol.flags.is(Flags.Case) then

          // ===
          // ===  Case Classes
          // ===
          val caseFields: List[FieldInfoRef] = constructorParams.zipWithIndex.map { (definition, idx) =>
            val valDef = definition.asInstanceOf[ValDef]
            implicit val q = quotes
            valDef.tpt.tpe.asType match
              case '[t] =>
                val fieldRef = unwindFieldRTypeWithTypeSubstitution[t](quotes)(
                  valDef,
                  symbol.caseFields(idx),
                  resolveTypeSyms,
                  typeRef
                )
                ReflectOnField(quotes)(
                  fieldRef.asInstanceOf[RTypeRef[_]],
                  valDef,
                  idx,
                  dad,
                  fieldDefaultMethods
                )
          }

          ScalaClassRef[T](
            className,
            typedName,
            typeSymbols,
            typeSymbolValues,
            typeMembers,
            caseFields,
            classAnnos,
            classDef.parents.map(_.asInstanceOf[TypeTree].tpe.classSymbol.get.fullName),
            typeSymbols.nonEmpty,
            isValueClass,
            true,
            isAbstract,
            typeParamPaths,
            Nil,
            sealedChildrenRTypes
          )(using quotes)(using typeRef.asType.asInstanceOf[Type[T]])
        else
          UnknownRef[T](symbol.fullName)(using quotes)(using typeRef.asType.asInstanceOf[Type[T]])

          /*
        else

          // ===
          // ===  Non-Case Classes
          // ===

          // Include inherited methods (var & def), including inherited!
          // Produces (val <field>, method <field>_=)
          // val varAnnos = scala.collection.mutable.Map.empty[String,Map[String, Map[String,String]]]
          val nonConstructorFields: List[NonConstructorFieldInfo] =
            ReflectOnField.nonCaseScalaField(quotes)(symbol, typeRef.asInstanceOf[TypeRepr])

          // ensure all constructur fields are vals
          val constructorFields = symbol.declaredFields
            .filter(_.flags.is(Flags.ParamAccessor))
            .zipWithIndex
            .map { (oneField, idx) =>
              val valDef = constructorParams(idx).asInstanceOf[ValDef]
              val fieldType = unwindFieldRTypeWithTypeSubstitution(quotes)(
                valDef,
                oneField,
                resolveTypeSyms,
                typeRef
              )
              val fieldtt = fieldType.toType(quotes)
              ReflectOnField(quotes)(
                fieldType,
                valDef,
                idx,
                dad,
                fieldDefaultMethods,
                oneField.flags.is(Flags.PrivateLocal)
              )(using fieldtt)
            }

          ScalaClassRType(
            className,
            typedName,
            typeSymbols,
            typeSymbolValues,
            typeMembers,
            constructorFields,
            classAnnos,
            classDef.parents.map(_.asInstanceOf[TypeTree].tpe.classSymbol.get.fullName),
            typeSymbols.nonEmpty,
            isValueClass,
            false,
            isAbstract,
            typeParamPaths,
            nonConstructorFields,
            sealedChildrenRTypes
          )
           */

      // === Other kinds of classes (non-case Scala) ===
      else UnknownRef[T](symbol.fullName)(using quotes)(using typeRef.asType.asInstanceOf[Type[T]])

  // Unwind RType for given constructor field--and do type subsititution if available (ie T->String)
  def unwindFieldRTypeWithTypeSubstitution[U](quotes: Quotes)(
      valDef: quotes.reflect.ValDef,
      fieldSymbol: quotes.reflect.Symbol,
      resolveTypeSyms: Boolean,
      classTypeRef: quotes.reflect.TypeRef
  )(using utt: Type[U])(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[?] =
    import quotes.reflect.*

    if resolveTypeSyms then
      reflect.ReflectOnType[U](quotes)(classTypeRef.memberType(fieldSymbol)) match {
        case tsym if tsym.name == NONE =>
          TypeSymbolRef(valDef.tpt.tpe.typeSymbol.name)(using quotes)
        case other => other
      }
    else if valDef.tpt.tpe.typeSymbol.flags.is(Flags.Param) then TypeSymbolRef(valDef.tpt.tpe.typeSymbol.name)(using quotes)
    else reflect.ReflectOnType[U](quotes)(valDef.tpt.tpe, false)
