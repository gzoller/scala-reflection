package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import scala.util.matching.Regex
import rtypes.*

/**
  * This is the workhorse of Scala reflection.  Dive into all the Scala-internal bits in the reflection API, extract the 
  * juicy bits, and construct our (hopefully) simpler RTypes.
  */
object ReflectOnClass:

  def apply(quotes: Quotes)(typeRef: quotes.reflect.TypeRef, typedName: TypedName, resolveTypeSyms: Boolean, appliedTob: List[quotes.reflect.TypeRepr] =  Nil): RType[_] = 
    import quotes.reflect.*

    //===
    //===  Preparation
    //===

    // Let's get the className, symbol, and any typeSymbols (eg Foo[T,U]) that may exist.
    // The .replace here "fixes" wrong class name in the case where a class is defined inside an object
    val className = typeRef.classSymbol.get.fullName.replace("$.","$")
    val symbol = typeRef.classSymbol.get
    val typeSymbols = symbol.primaryConstructor.paramSymss match {
      case List(paramSyms: List[Symbol], _) => paramSyms.map(_.name.asInstanceOf[TypeSymbol])
      case _ => Nil
    }

    // Something to allow us to pattern match and extract on default value methods in companion object...
    object DefaultMethod {
      val reg: Regex = """\$lessinit\$greater\$default\$(\d+)""".r
      def unapply(s: quotes.reflect.Symbol): Option[Int] = reg.findFirstIn(s.toString) match {
        case Some(reg(a)) => Some(a.toInt)
        case _ => None
      }
    }

    // Class annotations -> annotation map
    val annoSymbol = symbol.annotations.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
    val classAnnos = annoSymbol.map{ a =>
      val quotes.reflect.Apply(_, params) = a: @unchecked
      val annoName = a.symbol.signature.resultSig
      (annoName, annoSymToString(quotes)(params))
    }.toMap

    // Is this class annotated to skip reflection?   If so, return UnknownInfo
    if classAnnos.contains("co.blocke.scala_reflection.Skip_Reflection") then
      UnknownRType(symbol.fullName)

    else 
      
      //===
      //===  Class Reflection begins...
      //===

      // === Scala 2 Classes ===
      if symbol.flags.is(quotes.reflect.Flags.Scala2x) then
        UnknownRType(symbol.fullName)
        // symbol.fullName match {
        //   case PrimitiveType(t) => t
        //   case s => Scala2Info(s)
        // }

      else if symbol.flags.is(quotes.reflect.Flags.Trait) then
        // === Trait ===
        //     >> Sealed Traits
        if symbol.flags.is(quotes.reflect.Flags.Sealed) then
          val kidsRTypes = symbol.children.map{ c =>
            c.tree match {
              case vd: ValDef =>
                RType.unwindType(quotes)(vd.tpt.tpe)
                ObjectRType(vd.symbol.fullName)  // sealed object implementation
              case _ =>   // sealed case class implementation
                val typeDef: dotty.tools.dotc.ast.Trees.TypeDef[_] = c.tree.asInstanceOf[dotty.tools.dotc.ast.Trees.TypeDef[_]]
                RType.unwindType(quotes)(typeDef.typeOpt.asInstanceOf[quotes.reflect.TypeRepr])
            }
          }
          SealedTraitRType(className, kidsRTypes)
        else
          //  >> Normal (unsealed) traits
          typeRef match {
            case a @ AppliedType(t,tob) =>  // parameterized trait
              val actualParamTypes = tob.map{ oneTob =>
                scala.util.Try{
                  if resolveTypeSyms then
                    RType.unwindType(quotes)(oneTob.asInstanceOf[quotes.reflect.TypeRef])
                  else if oneTob.asInstanceOf[quotes.reflect.TypeRef].typeSymbol.flags.is(Flags.Param) then
                    TypeSymbolRType(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)
                  else
                    RType.unwindType(quotes)(oneTob.asInstanceOf[quotes.reflect.TypeRef], false)
                }.toOption.getOrElse{
                  TypeSymbolRType(oneTob.asInstanceOf[quotes.reflect.TypeRef].name)
                }
              }
              val paramMap: Map[TypeSymbol, RType[_]] = typeSymbols.zip(actualParamTypes).toMap

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
              TraitRType(
                className,
                traitFields,
                typeSymbols
              )
            case _ =>
              // non-parameterized trait
              val traitFields = symbol.tree.asInstanceOf[ClassDef].body.collect {
                case valDef: ValDef =>
                  val fieldType = RType.unwindType(quotes)(typeRef.memberType(valDef.symbol))
                  ScalaFieldInfo(-1, valDef.name, fieldType, Map.empty[String,Map[String,String]], None, None, true)
              }
              TraitRType(className, traitFields)
          }

          /*
      else if symbol.flags.is(quotes.reflect.Flags.Enum) then // Found top-level enum (i.e. not part of a class), e.g. member of a collection
        val enumClassSymbol = typeRef.classSymbol.get
        enumClassSymbol.companionClass.declaredMethods // <-- This shouldn't "do" anything!  For some reason it is needed or Enums test explodes.
        val enumValues = enumClassSymbol.children.map(_.name)
        ScalaEnumInfo(symbol.fullName, enumValues.toArray)

      // === Java Class ===
      // User-written Java classes will have the source file.  Java library files will have <no file> for source
      else if symbol.flags.is(Flags.JavaDefined) then
        // Reflecting Java classes requires the materialized Class, which may be available (e.g. Java collections) or not (e.g. user-written class).
        // So for now just burp forth a proxy and we'll resolve the details at runtime.
        JavaClassInfo(symbol.fullName, symbol.fullName, typeSymbols.toArray, appliedTob.map( at => RType.unwindType(quotes)(at.asInstanceOf[TypeRef])).toArray )
        */

      // === Scala 3 Classes ===
      else if symbol.isClassDef then
        val classDef = symbol.tree.asInstanceOf[ClassDef]

        val isValueClass = typeRef.baseClasses.contains(Symbol.classSymbol("scala.AnyVal"))

        // Get superclasses
        val dad = classDef.parents.headOption match {
          case Some(tt: TypeTree) if !isValueClass && tt.tpe.classSymbol.get.fullName != "java.lang.Object" =>
            ReflectOnClass(quotes)(tt.tpe.asInstanceOf[TypeRef], RType.typeName(quotes)(tt.tpe), resolveTypeSyms) match {
              case ci: ClassRType[_] => Some(ci) // Any kind of class
              case _ => None // e.g. Unknown
            }
          case _ => None
        }

        // Get any case field default value accessor method names (map by field index)
        // Question: What does it take to get Expr[Any]?  If doable, we can go ahead and get the default value right here and poke into the FieldInfo!
        val fieldDefaultMethods = symbol.companionClass match {
          case dotty.tools.dotc.core.Symbols.NoSymbol => Map.empty[Int, (String,String)]
          case s: Symbol => symbol.companionClass.declaredMethods.collect {
            case DefaultMethod(defaultIndex) => defaultIndex-1 -> (className+"$", ("$lessinit$greater$default$"+defaultIndex))
          }.toMap
        }

        // All this mucking around in the constructor.... why not just get the case fields from the symbol?
        // Because symbol's case fields lose the annotations!  Pulling from contstructor ensures they are retained.
        val tob = typeRef match {
          case AppliedType(t,tob) => tob
          case _                  => Nil
        }
        val typeMembers = classDef.body.collect {
          case TypeDef(typeName, typeTree) if typeSymbols.contains(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name) =>
            val pos = typeSymbols.indexOf(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name)
            TypeMemberRType(
              typeName,
              typeSymbols(pos),
              RType.unwindType(quotes)(tob(pos).asInstanceOf[TypeRepr])
            )
          case TypeDef(typeName, typeTree) if !typeSymbols.contains(typeName) => // type memebers not used in the constructor
            val tt = typeTree.asInstanceOf[TypeTree].tpe
            TypeMemberRType(
              typeName,
              typeName.asInstanceOf[TypeSymbol],
              RType.unwindType(quotes)(tt) // tob(pos).asInstanceOf[TypeRepr])
            )
        }

        val constructorParams =  // Annoying "ism"... different param order dep on whether class is parameterized or not!
          if classDef.constructor.paramss.tail == Nil then
            classDef.constructor.paramss.head.params
          else
            classDef.constructor.paramss.tail.head.params

        if symbol.flags.is(quotes.reflect.Flags.Case) then
          // === Case Classes ===
          val caseFields = constructorParams.zipWithIndex.map{ (definition, idx) =>
            val valDef = definition.asInstanceOf[ValDef]
            val fieldRType = scala.util.Try{
              if resolveTypeSyms then
                RType.unwindType(quotes)(typeRef.memberType(symbol.caseFields(idx))) match {
                  case NONE  =>
                    TypeSymbolRType(valDef.tpt.tpe.typeSymbol.name)
                  case other => other
                }
              else if valDef.tpt.tpe.typeSymbol.flags.is(Flags.Param) then
                TypeSymbolRType(valDef.tpt.tpe.typeSymbol.name)
              else
                RType.unwindType(quotes)(valDef.tpt.tpe, false)
            }.toOption.getOrElse{
              TypeSymbolRType(valDef.tpt.tpe.typeSymbol.name)
            }
            val fieldtt = valDef.tpt.tpe.asType.asInstanceOf[Type[fieldRType.T]]
            reflectOnField(quotes)(fieldRType, valDef, idx, dad, fieldDefaultMethods)(using fieldtt)
          }

          var returnedRType = ScalaClassRType(
            className,
            typedName,
            // {if typeSymbols.nonEmpty then className + actualParamTypes.map(t => t.name).mkString("[",",","]") else fullName},
            typeSymbols,
            typeMembers,
            caseFields,
            classAnnos,
            classDef.parents.map(_.symbol.fullName),
            typeSymbols.nonEmpty,
            isValueClass,
            true
            )

            // RType.quotedTypeCache.put(returnedRType.typedName, classDef.symbol.typeRef.asType) // register the quoted.Type into the cache
            returnedRType

        else
          UnknownRType(className)

            /*
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
            */

      // === Other kinds of classes (non-case Scala) ===
      else
        UnknownRType(symbol.fullName)


  def reflectOnField[Q](quotes: Quotes)(
    fieldType: RType[Q],
    valDef: quotes.reflect.ValDef, 
    index: Int, 
    dad: Option[ClassRType[_]],
    fieldDefaultMethods: Map[Int, (String,String)],
    isNonValConstructorField: Boolean = false
  )(using Type[Q]): FieldInfo = 
    import quotes.reflect.* 

    // Get any field annotations (from body of class--they're not on the constructor fields)
    val fieldAnnos = dad match {
      // case Some(c) if c.isInstanceOf[JavaClassInfo] => Map.empty[String,Map[String,String]]
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

    ScalaFieldInfo(
      index, 
      valDef.name, 
      fieldType, 
      fieldAnnos,
      fieldDefaultMethods.get(index),
      originalTypeSymbol,
      isNonValConstructorField
      )


inline def annoSymToString(quotes: Quotes)( terms: List[quotes.reflect.Term] ): Map[String,String] =
  import quotes.reflect._
  terms.collect {
    case NamedArg(argName, Literal(BooleanConstant(argValue))) => (argName -> argValue.toString)
    case NamedArg(argName, Literal(ByteConstant(argValue)))    => (argName -> argValue.toString)
    case NamedArg(argName, Literal(ShortConstant(argValue)))   => (argName -> argValue.toString)
    case NamedArg(argName, Literal(CharConstant(argValue)))    => (argName -> argValue.toString)
    case NamedArg(argName, Literal(IntConstant(argValue)))     => (argName -> argValue.toString)
    case NamedArg(argName, Literal(LongConstant(argValue)))    => (argName -> argValue.toString)
    case NamedArg(argName, Literal(FloatConstant(argValue)))   => (argName -> argValue.toString)
    case NamedArg(argName, Literal(DoubleConstant(argValue)))  => (argName -> argValue.toString)
    case NamedArg(argName, Literal(StringConstant(argValue)))  => (argName -> argValue)
  }.toMap