package co.blocke.scala_reflection
package impl

import info._
import scala.quoted.Quotes
import scala.util.control.Breaks._


trait NonCaseClassReflection:

  def inspectNonCaseClass(quotes: Quotes)(
    symbol:                quotes.reflect.Symbol,
    tob:                   List[quotes.reflect.TypeRepr],
    paramSymbols:          Array[TypeSymbol],
    classDef:              quotes.reflect.ClassDef,
    superClass:            Option[ClassInfo],
    name:                  String,
    fullName:              String,
    isAppliedType:         Boolean,
    fieldDefaultMethods:   Map[Int, (String,String)],
    typeMembers:           Array[TypeMemberInfo],
    fields:                Array[FieldInfo],
    annotations:           Map[String, Map[String,String]],
    paths:                 Map[String, Map[String,List[Int]]],
    mixins:                Array[String],
    isValueClass:          Boolean
  ): ScalaClassInfo = 
    import quotes.reflect.{_, given}

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
                val quotes.reflect.Apply(_, params) = a
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

    val numConstructorFields = fields.length

    // Include inherited methods (var & def), including inherited!
    val baseAnnos = superClass match {
      case Some(c: ScalaClassInfo) => c.nonConstructorFields.map( f => f.name -> (f.annotations - S3ANNO) ).toMap
      case _ => Map.empty[String,Map[String, Map[String,String]]]
    }

    // Include inherited methods (var & def), including inherited!
    // Produces (val <field>, method <field>_=)
    val getterSetter: List[(Symbol,Symbol)] = symbol.memberMethods.filter(_.name.endsWith("_=")).map{ setter => 
      // Trying to get the setter... which could be a val (field) if declared is a var, or it could be a method 
      // in the case of user-written getter/setter... OR it could be defined in the superclass
      symbol.memberField(setter.name.dropRight(2)) match {
        case dotty.tools.dotc.core.Symbols.NoSymbol => 
          symbol.memberMethod(setter.name.dropRight(2)) match {
            case Nil => 
              throw new ReflectException(s"Can't find field getter ${setter.name.dropRight(2)} in class ${symbol.fullName} or its superclass(es).")
            case getter => 
              (getter.head, setter)
          }
        case getter: Symbol => 
          (getter, setter)
      }
    }

    val knownAnnos = baseAnnos ++ getterSetter.map{ (fGet, fSet) =>
      val both = fGet.annotations ++ fSet.annotations
      val annoMap = both.map{ a => 
        val quotes.reflect.Apply(_, params) = a
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
        originalTypeSymbol.map( ots => RType.unwindType(quotes)(tob(typeSymbols.indexOf(ots)).asInstanceOf[TypeRepr]) ).getOrElse{
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
        knownAnnos(fieldName).toMap,
        None, // we don't know how to get the default values (initial set values) of non-constructor fields at present
        originalTypeSymbol.map(_.asInstanceOf[TypeSymbol]),
        true
      )
    }.toArray

    ScalaClassInfo(
      name,
      fullName,
      paramSymbols,
      typeMembers.toArray,
      fields,
      nonConstructorFields,
      annotations,
      paths,
      mixins,
      isAppliedType,
      isValueClass
    )
