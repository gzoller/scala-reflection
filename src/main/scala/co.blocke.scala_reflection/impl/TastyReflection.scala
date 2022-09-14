package co.blocke.scala_reflection
package impl

import info.*
import scala.quoted.*
import scala.reflect.*
import scala.quoted.Quotes
import scala.util.Try


inline def annoSymToString(quotes: Quotes)( terms: List[quotes.reflect.Term] ): Map[String,String] =
  import quotes.reflect._
  terms.collect {
    case NamedArg(argName, Literal(BooleanConstant(argValue))) => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(ByteConstant(argValue)))    => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(ShortConstant(argValue)))   => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(CharConstant(argValue)))    => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(IntConstant(argValue)))     => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(LongConstant(argValue)))    => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(FloatConstant(argValue)))   => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(DoubleConstant(argValue)))  => (argName.toString -> argValue.toString)
    case NamedArg(argName, Literal(StringConstant(argValue)))  => (argName.toString -> argValue)
  }.toMap


object TastyReflection extends NonCaseClassReflection:

  def reflectOnType(quotes: Quotes)(aType: quotes.reflect.TypeRepr, fullName: String, resolveTypeSyms: Boolean): RType = 
    import quotes.reflect.{_, given}

    val typeRef = aType.asInstanceOf[TypeRef]
    typeRef.classSymbol match {

      // Intersection types don't have a class symbol, so don't assume one!
      case None =>
        typeRef match {
          // Intersection Type
          //----------------------------------------
          case AndType(left,right) =>
            val resolvedLeft: RType = RType.unwindType(quotes)(left.asInstanceOf[quotes.reflect.TypeRef])
            val resolvedRight: RType = RType.unwindType(quotes)(right.asInstanceOf[quotes.reflect.TypeRef])
            IntersectionInfo(INTERSECTION_CLASS, resolvedLeft, resolvedRight)

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft: RType = RType.unwindType(quotes)(left.asInstanceOf[quotes.reflect.TypeRef])
            val resolvedRight: RType = RType.unwindType(quotes)(right.asInstanceOf[quotes.reflect.TypeRef])
            UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)

          case u => 
            throw new ReflectException("Unsupported TypeRef: "+typeRef)
        }

    case Some(classSymbol) =>
      // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
      val (is2xEnumeration, className) = classSymbol.fullName match { 
        case raw if raw == ENUM_CLASSNAME => 
          val enumerationClass = typeRef.typeSymbol.fullName
          if( enumerationClass == ENUM_CLASSNAME ) then
            // If caller did NOT define a type member (type X = Value) inside their Enumeration class
            val enumClassName = typeRef.qualifier.asInstanceOf[quotes.reflect.TermRef].termSymbol.moduleClass.fullName.dropRight(1) // chop the '$' off the end!
            (true, enumClassName)
          else
            // If caller defined a type member (type X = Value) inside their Enumeration class
            (true, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
        case _  => (false, classSymbol.fullName)
      }

      typeRef match {
        case named: dotty.tools.dotc.core.Types.NamedType if classSymbol == Symbol.classSymbol("scala.Any") =>
          // Scala3 opaque type alias
          //----------------------------------------
          if typeRef.isOpaqueAlias then
            val translucentSuperType = typeRef.translucentSuperType
            AliasInfo(typeRef.show, RType.unwindType(quotes)(translucentSuperType))

          // Any Type
          //----------------------------------------
          else
            PrimitiveType.Scala_Any

        // Scala3 Tasty-equipped type incl. primitive types
        // Traits and classes w/type parameters are *not* here... they're AppliedTypes
        //----------------------------------------
        case named: dotty.tools.dotc.core.Types.NamedType => 
          val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
          classSymbol match {
            case cs if isTypeParam => 
              TypeSymbolInfo(typeRef.name)  // TypeSymbols Foo[T] have typeRef of Any
            case cs if is2xEnumeration => 
              val enumerationClassSymbol = typeRef.qualifier.asInstanceOf[quotes.reflect.TermRef].termSymbol.moduleClass
              ScalaEnumerationInfo(enumerationClassSymbol.fullName.dropRight(1), enumerationClassSymbol.declaredFields.map( _.name ).toArray)  // get the values of the Enumeration
            case cs => 
              // Primitive type test:
              PrimitiveType.values.find(_.name == className).getOrElse{
                reflectOnClass(quotes)(typeRef, RType.typeName(quotes)(typeRef), resolveTypeSyms)
              }
          }

        // Union Type
        //----------------------------------------
        case OrType(left,right) =>
          val resolvedLeft = RType.unwindType(quotes)(left.asInstanceOf[quotes.reflect.TypeRef])
          val resolvedRight = RType.unwindType(quotes)(right.asInstanceOf[quotes.reflect.TypeRef])
          UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)
      
        // Parameterized Types (classes, traits, & collections)
        //----------------------------------------
        case a @ AppliedType(t,tob) => 
          // First see if we have some sort of collection or other "wrapped" type
          val foundType: Option[RType] = extractors.ExtractorRegistry.extractors.collectFirst {
            case e if e.matches(quotes)(classSymbol) => 
              e.extractInfo(quotes)(t, tob, classSymbol)
          }
          foundType.getOrElse {
            // Nope--we've got a parameterized class or trait here
            reflectOnClass(quotes)(a.asInstanceOf[TypeRef], RType.typeName(quotes)(a), resolveTypeSyms, tob)
          }
      
        case x => 
          // === No idea!  Unkonwn entity...
          UnknownInfo(className)
      }
    }


  def reflectOnClass(quotes: Quotes)(typeRef: quotes.reflect.TypeRef, fullName: String, resolveTypeSyms: Boolean, appliedTob: List[quotes.reflect.TypeRepr] =  Nil): RType = 
    import quotes.reflect.{_, given}

    // The .replace here "fixes" wrong class name in the case where a class is defined inside an object
    val className = typeRef.classSymbol.get.fullName.replace("$.","$")

    object DefaultMethod {
      val reg = """\$lessinit\$greater\$default\$(\d+)""".r
      def unapply(s: quotes.reflect.Symbol): Option[Int] = reg.findFirstIn(s.toString) match {
        case Some(reg(a)) => Some(a.toInt)
        case _ => None
      }
    }

    val symbol = typeRef.classSymbol.get
    val typeSymbols = symbol.primaryConstructor.paramSymss match {
      case List(paramSyms: List[Symbol], _) => paramSyms.map(_.name.asInstanceOf[TypeSymbol])
      case _ => Nil
    }

    // Class annotations -> annotation map
    val annoSymbol = symbol.annotations.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
    val classAnnos = annoSymbol.map{ a =>
      val quotes.reflect.Apply(_, params) = a: @unchecked
      val annoName = a.symbol.signature.resultSig
      (annoName, annoSymToString(quotes)(params))
    }.toMap

    // Skip this class?   If so, return UnknownInfo
    if classAnnos.contains("co.blocke.scala_reflection.Skip_Reflection") then
      UnknownInfo(symbol.fullName)
    else // proceed with reflection...

      if symbol.flags.is(quotes.reflect.Flags.Scala2x) then
        symbol.fullName match {
          case PrimitiveType(t) => t
          case s => Scala2Info(s)
        }

      else if symbol.flags.is(quotes.reflect.Flags.Trait) then
        // === Trait ===
        //     >> Sealed Traits
        if symbol.flags.is(quotes.reflect.Flags.Sealed) then
          val kidsRTypes = symbol.children.map{ c =>
            c.tree match {
              case vd: ValDef =>
                RType.unwindType(quotes)(vd.tpt.tpe)
                ObjectInfo(vd.symbol.fullName)  // sealed object implementation
              case _ =>   // sealed case class implementation
                val typeDef: dotty.tools.dotc.ast.Trees.TypeDef[_] = c.tree.asInstanceOf[dotty.tools.dotc.ast.Trees.TypeDef[_]]
                RType.unwindType(quotes)(typeDef.typeOpt.asInstanceOf[quotes.reflect.TypeRepr])
            }
          }
          SealedTraitInfo(
            className,
            kidsRTypes.toArray)
        else
          //  >> Normal (unsealed) traits
          typeRef match {
            case a @ AppliedType(t,tob) =>  // parameterized trait
              val actualParamTypes = tob.map{ oneTob =>
                scala.util.Try{
                  if resolveTypeSyms then
                    RType.unwindType(quotes)(oneTob.asInstanceOf[quotes.reflect.TypeRef])
                  else if oneTob.asInstanceOf[quotes.reflect.TypeRef].typeSymbol.flags.is(Flags.Param) then
                    TypeSymbolInfo(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)
                  else
                    RType.unwindType(quotes)(oneTob.asInstanceOf[quotes.reflect.TypeRef], false)
                }.toOption.getOrElse{
                  TypeSymbolInfo(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)
                }
              }
              val paramMap: Map[TypeSymbol, RType] = typeSymbols.zip(actualParamTypes).toMap

              val traitFields = symbol.declaredFields.zipWithIndex.map { (f,index) =>
                val fieldType =
                  // A lot of complex messiness to sew down the mapped type:  Foo[T]( val x: List[T]) where we call Foo[W] from a higher class,
                  // so T -> W.  We need resolveTypeParams to sew down for deeper nested types like List.  Ugh.
                  scala.util.Try{
                    if resolveTypeSyms then
                      RType.unwindType(quotes)(typeRef.memberType(f))
                    else
                      f.tree match {
                        case vd: ValDef if vd.tpt.tpe.typeSymbol.flags.is(Flags.Param) =>
                          paramMap.getOrElse(
                            vd.tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol],
                            RType.unwindType(quotes)(vd.tpt.tpe).asInstanceOf[AppliedRType].resolveTypeParams(paramMap)
                          )
                        case _ =>
                          RType.unwindType(quotes)(typeRef.memberType(f), false)
                      }
                  }.toOption.getOrElse{
                    paramMap.getOrElse(
                      f.tree.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol],
                      RType.unwindType(quotes)(f.tree.asInstanceOf[ValDef].tpt.tpe).asInstanceOf[AppliedRType].resolveTypeParams(paramMap)
                    )
                  }
                val typeSym =
                  f.tree.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol] match {
                    case ts if typeSymbols.contains(ts) => Some(ts)
                    case _ => None
                  }
                ScalaFieldInfo(index, f.name, fieldType, Map.empty[String,Map[String,String]], None, typeSym, true)
              }
              TraitInfo(
                className,
                traitFields.toArray,
                actualParamTypes.toArray,
                typeSymbols.toArray
              )
            case _ =>
              // non-parameterized trait
              val traitFields = symbol.tree.asInstanceOf[ClassDef].body.collect {
                case valDef: ValDef =>
                  val fieldType = RType.unwindType(quotes)(typeRef.memberType(valDef.symbol))
                  ScalaFieldInfo(-1, valDef.name, fieldType, Map.empty[String,Map[String,String]], None, None, true)
              }
              TraitInfo(className, traitFields.toArray)
          }

      else if symbol.flags.is(quotes.reflect.Flags.Enum) then // Found top-level enum (i.e. not part of a class), e.g. member of a collection
        val enumClassSymbol = typeRef.classSymbol.get
        enumClassSymbol.companionClass.declaredMethods // <-- This shouldn't "do" anything!  For some reason it is needed or Enums test explodes.
        val enumValues = enumClassSymbol.children.map(_.name)
        ScalaEnumInfo(symbol.fullName, enumValues.toArray)

      // === Java Class ===
      // User-written Java classes will have the source file.  Java library files will have <no file> for source
      else if symbol.flags.is(Flags.JavaDefined) then
        // Reflecting Java classes requires the materialized Class, which may be available (e.g. Java collections) or not (e.g. user-written class).
        // So for now just burp forth a proxy and we'll resovle the details at runtime.
        JavaClassInfo(symbol.fullName, symbol.fullName, typeSymbols.toArray, appliedTob.map( at => RType.unwindType(quotes)(at.asInstanceOf[TypeRef])).toArray )

      // === Scala Classes ===
      else if symbol.isClassDef then
        // Get field annotatations (from body of class--they're not on the constructor fields)
        val classDef = symbol.tree.asInstanceOf[ClassDef]

        val isValueClass = typeRef.baseClasses.contains(Symbol.classSymbol("scala.AnyVal"))

        // Get superclass' field annotations--if any
        val dad = classDef.parents.headOption match {
          case Some(tt: TypeTree) if !isValueClass && tt.tpe.classSymbol.get.fullName != "java.lang.Object" =>
            reflectOnClass(quotes)(tt.tpe.asInstanceOf[TypeRef], RType.typeName(quotes)(tt.tpe), resolveTypeSyms) match {
              case ci: ClassInfo => Some(ci) // Any kind of class
              case _ => None // e.g. Unknown
            }
          case _ => None
        }

        // Get any case field default value accessor method names (map by field index)
        val fieldDefaultMethods = symbol.companionClass match {
          case dotty.tools.dotc.core.Symbols.NoSymbol => Map.empty[Int, (String,String)]
          case s: Symbol => symbol.companionClass.declaredMethods.collect {
            case DefaultMethod(defaultIndex) => defaultIndex-1 -> (className+"$", ("$lessinit$greater$default$"+defaultIndex))
          }.toMap
        }

        // All this mucking around in the constructor.... why not just get the case fields from the symbol?
        // Because:  symbol's case fields lose the annotations!  Pulling from contstructor ensures they are retained.
        val tob = typeRef match {
          case AppliedType(t,tob) => tob
          case _                  => Nil
        }
        val typeMembers = classDef.body.collect {
          case TypeDef(typeName, typeTree) if typeSymbols.contains(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name) =>
            val pos = typeSymbols.indexOf(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name)
            TypeMemberInfo(
              typeName,
              typeSymbols(pos),
              RType.unwindType(quotes)(tob(pos).asInstanceOf[TypeRepr])
            )
        }

        val actualParamTypes = tob.zipWithIndex.map{ (oneTob,idx) =>
          scala.util.Try{
            if resolveTypeSyms then
              RType.unwindType(quotes)(oneTob.asInstanceOf[quotes.reflect.TypeRef]) match {
                case NONE  =>
                  TypeSymbolInfo(typeSymbols(idx).toString)
                case other => other
              }
            else if oneTob.asInstanceOf[quotes.reflect.TypeRef].typeSymbol.flags.is(Flags.Param) then
              TypeSymbolInfo(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)
            else
              RType.unwindType(quotes)(oneTob.asInstanceOf[quotes.reflect.TypeRef], false)
          }.toOption.getOrElse{
            TypeSymbolInfo(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)
          }
        }
        val paramMap: Map[TypeSymbol, RType] = typeSymbols.zip(actualParamTypes).toMap

        val constructorParams =  // Annoying "ism"... different param order dep on whether class is parameterized or not!
          if classDef.constructor.paramss.tail == Nil then
            classDef.constructor.paramss.head.params
          else
            classDef.constructor.paramss.tail.head.params

        if symbol.flags.is(quotes.reflect.Flags.Case) then

          // === Case Classes ===
          val caseFields = constructorParams.zipWithIndex.map{ (definition, idx) =>
            val valDef = definition.asInstanceOf[ValDef]
            val fieldType = scala.util.Try{
              if resolveTypeSyms then
                RType.unwindType(quotes)(typeRef.memberType(symbol.caseFields(idx))) match {
                  case NONE  =>
                    TypeSymbolInfo(valDef.tpt.tpe.typeSymbol.name)
                  case other => other
                }
              else if valDef.tpt.tpe.typeSymbol.flags.is(Flags.Param) then
                TypeSymbolInfo(valDef.tpt.tpe.typeSymbol.name)
              else
                RType.unwindType(quotes)(valDef.tpt.tpe, false)
            }.toOption.getOrElse{
              TypeSymbolInfo(valDef.tpt.tpe.typeSymbol.name)
            }
            reflectOnField(quotes)(fieldType, valDef, idx, dad, fieldDefaultMethods).resolveTypeParams( paramMap )
          }

          ScalaCaseClassInfo(
            className,
            fullName,
            // {if typeSymbols.nonEmpty then className + actualParamTypes.map(t => t.name).mkString("[",",","]") else fullName},
            typeSymbols.toArray,
            typeMembers.toArray,
            caseFields.toArray,
            classAnnos,
            TypeLoom.descendParents(quotes)( typeRef ),
            classDef.parents.map(_.symbol.fullName).toArray,
            typeSymbols.nonEmpty,
            isValueClass)
        else
          // === Non-Case Classes ===

          // ensure all constructur fields are vals
          val classFields = symbol.declaredFields.filter( _.flags.is(Flags.ParamAccessor))
            .zipWithIndex
            .map{ (oneField, idx) =>
              val fieldType = RType.unwindType(quotes)(typeRef.memberType(oneField))
              reflectOnField(quotes)(fieldType, constructorParams(idx).asInstanceOf[ValDef], idx, dad, fieldDefaultMethods, oneField.flags.is(Flags.PrivateLocal))
            }

          inspectNonCaseClass(quotes)(
            symbol,
            tob,
            typeSymbols.toArray,
            classDef,
            dad,
            className,
            fullName,
            typeSymbols.nonEmpty,
            fieldDefaultMethods,
            typeMembers.toArray,
            classFields.toArray,
            classAnnos,
            TypeLoom.descendParents(quotes)( typeRef ),
            classDef.parents.map(_.symbol.fullName).toArray,
            isValueClass)

      // === Other kinds of classes (non-case Scala) ===
      else
        UnknownInfo(symbol.fullName)


  def reflectOnField(quotes: Quotes)(
    fieldType: RType,
    valDef: quotes.reflect.ValDef, 
    index: Int, 
    dad: Option[ClassInfo],
    fieldDefaultMethods: Map[Int, (String,String)],
    isNonValConstructorField: Boolean = false
  ): FieldInfo = 
    import quotes.reflect.{_, given}
    val fieldAnnos = dad match {
      case Some(c) if c.isInstanceOf[JavaClassInfo] => Map.empty[String,Map[String,String]]
      case _ =>
        val baseAnnos = dad.flatMap( _.fields.find(_.name == valDef.name) ).map(_.annotations).getOrElse(Map.empty[String,Map[String,String]])
        baseAnnos ++ valDef.symbol.annotations.map{ a =>
          val quotes.reflect.Apply(_, params) = a: @unchecked
          val annoName = a.symbol.signature.resultSig
          (annoName, annoSymToString(quotes)(params))
        }.toMap
    }

    // Figure out the original type symbols, i.e. T, (if any)
    val valTypeRef = valDef.tpt.tpe.asInstanceOf[quotes.reflect.TypeRef]
    val isTypeParam = valTypeRef.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None

    ScalaFieldInfo(index, valDef.name, fieldType, fieldAnnos, fieldDefaultMethods.get(index), originalTypeSymbol, false, isNonValConstructorField)
