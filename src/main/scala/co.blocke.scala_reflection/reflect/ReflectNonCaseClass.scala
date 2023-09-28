package co.blocke.scala_reflection
package reflect

import rtypes.*
import scala.quoted.Quotes
import scala.util.control.Breaks.*

/**
  * Reflect on non-case Scala classes.  There are some patterns that must be respected:

  ALL WRONG NOW!!!!  -- update for changes!
  *
  *   --> All constructor arguments must be 'val' or they will not be visible to reflection
  *   --> Public var with @reflect annotation will be reflected.  All other vals and vars will not.
  *   --> Scala getter/setter pattern (below) will be reflected by default
  *
  *   class Foo( val visible: Int, val alsoVisible: String, notVisible: Float ):
  *      @reflect var isReflected: Boolean = true
  *      val nope: Int = 5
  *      var alsoNope: Int = 0
  *      private var againNope: Int = -1
  * 
  *      // Getter/setter pattern reflection looks for
  *      private var _age = 0
  *      def age = _age
  *      def age_=(g: Int): Unit = _age = g
  *      
  */
object ReflectNonCaseClass:

//  def apply[T](quotes: Quotes)(typeRef: quotes.reflect.TypeRef, typedName: TypedName, resolveTypeSyms: Boolean, appliedTob: List[quotes.reflect.TypeRepr] =  Nil): RType[T] = 

  def apply[T](quotes: Quotes)(
    name:                  String,
    typedName:             TypedName,
    symbol:                quotes.reflect.Symbol,
    constructorParams:     List[quotes.reflect.ValDef] | List[quotes.reflect.TypeDef],
    superClass:            Option[ClassRType[_]],
    fieldDefaultMethods:   Map[Int, (String,String)],

    // fields:                Array[FieldInfo],
    // tob:                   List[quotes.reflect.TypeRepr],
    // paramSymbols:          Array[TypeSymbol],
    // classDef:              quotes.reflect.ClassDef,
    // isAppliedType:         Boolean,
    // typeMembers:           Array[TypeMemberRType],
    // annotations:           Map[String, Map[String,String]],
    // paths:                 Map[String, Map[String,List[Int]]],
    // mixins:                Array[String],
    // isValueClass:          Boolean
  ): RType[T] = 
    import quotes.reflect.*

    /*
    var index: Int = fields.length - 1

    val fieldNames = fields.map(_.name)

    val varAnnos = scala.collection.mutable.Map.empty[String,Map[String, Map[String,String]]]
    val varDefDeclarations = classDef.body.collect{
        // We just want public var definitions here
        case s: ValDef if !s.symbol.flags.is(quotes.reflect.Flags.Private) 
          && !s.symbol.flags.is(quotes.reflect.Flags.Protected) 
          && !fieldNames.contains(s.name) 
          && s.symbol.flags.is(quotes.reflect.Flags.Mutable) => 
            val annoSymbol = s.symbol.annotations.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
            val fieldAnnos = 
              annoSymbol.map{ a => 
                val quotes.reflect.Apply(_, params) = a: @unchecked
                val annoName = a.symbol.signature.resultSig
                (annoName, annoSymToString(quotes)(params))

              }.toMap
            varAnnos.put(s.name, fieldAnnos) // yes, this is a side-effect but it saves mutliple field scans!
            s.name -> s.tpt.tpe.asInstanceOf[quotes.reflect.TypeRef]

        // We just want public def definitions here
        // WARNING: These defs may also include non-field functions!  Filter later...
        case d: DefDef if !d.symbol.flags.is(quotes.reflect.Flags.Private) 
          && !d.symbol.flags.is(quotes.reflect.Flags.Protected) 
          && !d.name.endsWith("_=") => d.name -> d.returnTpt.tpe.asInstanceOf[quotes.reflect.TypeRef]
    }.toMap

    // Include inherited methods (var & def), including inherited!
    val baseAnnos = superClass match {
      case Some(c: ScalaClassInfo) => c.nonConstructorFields.map( f => f.name -> (f.annotations - S3ANNO) ).toMap
      case _ => Map.empty[String,Map[String, Map[String,String]]]
    }
    */

    // Include inherited methods (var & def), including inherited!
    // Produces (val <field>, method <field>_=)
    val nonConstructorFields: List[NonConstructorFieldInfo] = symbol.methodMembers.filter(_.name.endsWith("_=")).map{ setter =>
      // Trying to get the setter... which could be a val (field) if declared is a var, or it could be a method 
      // in the case of user-written getter/setter... OR it could be defined in the superclass
      symbol.fieldMember(setter.name.dropRight(2)) match {
        case dotty.tools.dotc.core.Symbols.NoSymbol => 
          symbol.methodMember(setter.name.dropRight(2)) match {
            case Nil => 
              throw new ReflectException(s"Can't find field getter ${setter.name.dropRight(2)} in class ${symbol.fullName} or its superclass(es).")
            case getter => 
              (getter.head, setter)
          }
        case getter: Symbol => 
          (getter, setter)
      }
    }.filterNot{ (getterSym, setterSym) => 
      getterSym.annotations.map(_.tpe.typeSymbol.fullName).contains("co.blocke.scala_reflection.Ignore") ||
        setterSym.annotations.map(_.tpe.typeSymbol.fullName).contains("co.blocke.scala_reflection.Ignore")
    }.map{ case (getter, setter) =>
      val ftypeRepr = setter.tree.asInstanceOf[DefDef].paramss.head.params.head.asInstanceOf[ValDef].tpt.tpe
      NonConstructorFieldInfo(
        getter.name,
        setter.name,
        getter.isValDef,
        RType.unwindType(quotes)(ftypeRepr),
        Map.empty[String,Map[String,String]]
      )
    }

    // ensure all constructur fields are vals
    val constructorFields = symbol.declaredFields.filter( _.flags.is(Flags.ParamAccessor))
      .zipWithIndex
      .map{ (oneField, idx) =>
        val fieldType = RType.unwindType(quotes)(symbol.typeRef.memberType(oneField))
        val fieldtt = fieldType.toType(quotes)
        ReflectOnField(quotes)(fieldType, constructorParams(idx).asInstanceOf[ValDef], idx, superClass, fieldDefaultMethods, oneField.flags.is(Flags.PrivateLocal))(using fieldtt)
      }

    ScalaClassRType(
      name,
      typedName,
      Nil,
      Nil,
      Nil,
      constructorFields,
      Map.empty[String, Map[String,String]],
      Nil,
      false,
      false,
      false,
      Map.empty[String, List[List[Int]]],
      nonConstructorFields
    )

    /*
case class ScalaClassRType[R] (
    name:                   String,
    typedName:              TypedName,
    typeParamSymbols:       List[TypeSymbol],
    typeParamValues:        List[RType[_]],      // Like Int, Boolean
    _typeMembers:           List[TypeMemberRType],
    _fields:                List[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    override val isAppliedType: Boolean,
    isValueClass:           Boolean,
    isCaseClass:            Boolean,
    typeParamPaths:         Map[String,List[List[Int]]] = Map.empty[String,List[List[Int]]], // Trait/Class name -> List of Int (path) for each type param
    //nonConstructorFields:   List[NonConstructorFieldInfo] = Nil  // Populated for non-case classes only
) extends ClassRType[R]:

    */

    /*
    val knownAnnos = baseAnnos ++ getterSetter.map{ (fGet, fSet) =>
      val both = fGet.annotations ++ fSet.annotations
      val annoMap = both.map{ a => 
        val quotes.reflect.Apply(_, params) = a: @unchecked
        val annoName = a.symbol.signature.resultSig
        (annoName, annoSymToString(quotes)(params))
      }.toMap
      val allMap = 
        annoMap ++ varAnnos.getOrElse(fGet.name, Map.empty[String,Map[String,String]]) match {
          case m if m.isEmpty => baseAnnos.getOrElse(fGet.name, Map.empty[String,Map[String,String]])
          case m => m
        }
      (fGet.name -> allMap)
    }.toMap

    val typeSymbols = symbol.primaryConstructor.paramSymss match {
      case List(paramSyms: List[Symbol], _) => paramSyms.map(_.name)
      case _ => Nil
    }
    val nonConstructorFields = getterSetter.map { (fGet, fSet) =>
      val fieldName = fGet.name

      // Figure out the original type symbols, i.e. T, (if any)
      val originalTypeSymbol = { 
        val tpe: TypeRepr = fGet.tree match {
          case vd: ValDef => vd.tpt.tpe
          case dd: DefDef => dd.returnTpt.tpe
        }
        typeSymbols.find(_ == tpe.typeSymbol.name)
      }

      val rtype = 
        originalTypeSymbol.map( ots => RType.unwindType(quotes)(tob(typeSymbols.indexOf(ots))) ).getOrElse{
          if varDefDeclarations.contains(fieldName) then
            RType.unwindType(quotes)(varDefDeclarations(fieldName))
          else
            fGet.tree match {
              case dd: DefDef => RType.unwindType(quotes)(dd.returnTpt.tpe)
              case vd: ValDef => RType.unwindType(quotes)(vd.tpt.tpe)
            }
        }
  
      index += 1

      ScalaFieldInfo(
        index,
        fieldName,
        rtype,
        knownAnnos(fieldName),
        None, // we don't know how to get the default values (initial set values) of non-constructor fields at present
        originalTypeSymbol.map(_.asInstanceOf[TypeSymbol]),
        true
      )
    }.toArray

    val kidsRTypes = if symbol.flags.is(quotes.reflect.Flags.Sealed) then
      symbol.children.map { c =>
        c.tree match {
          case vd: ValDef =>
            RType.unwindType(quotes)(vd.tpt.tpe)
            ObjectInfo(vd.symbol.fullName) // sealed object implementation
          case _ => // sealed case class implementation
            val typeDef: dotty.tools.dotc.ast.Trees.TypeDef[_] = c.tree.asInstanceOf[dotty.tools.dotc.ast.Trees.TypeDef[_]]
            RType.unwindType(quotes)(typeDef.typeOpt.asInstanceOf[quotes.reflect.TypeRepr])
        }
      }
    else
      Seq.empty

    ScalaClassRType(
      name,
      fullName,
      typeParamSymbols,
      typeParamValues,
      typeMembers,
      fields,
      nonConstructorFields,
      annotations,
      mixins,
      kidsRTypes.toArray,
      isAppliedType,
      isValueClass,
      paths
    )
    */
    //UnknownRType("Boom").asInstanceOf[RType[T]]

    /*
    case class ScalaClassRType[R] (
    name:                   String,
    typedName:              TypedName,
    typeParamSymbols:       List[TypeSymbol],
    typeParamValues:        List[RType[_]],      // Like Int, Boolean
    _typeMembers:           List[TypeMemberRType],
    _fields:                List[FieldInfo],
    _annotations:           Map[String, Map[String,String]],
    _mixins:                List[String],
    override val isAppliedType: Boolean,
    isValueClass:           Boolean,
    isCaseClass:            Boolean,
    typeParamPaths:         Map[String,List[List[Int]]] = Map.empty[String,List[List[Int]]] // Trait/Class name -> List of Int (path) for each type param
) extends ClassRType[R]:
  */