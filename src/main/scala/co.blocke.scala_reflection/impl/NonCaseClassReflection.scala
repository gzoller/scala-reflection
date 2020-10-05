package co.blocke.scala_reflection
package impl

import info._
import scala.tasty.Reflection
import scala.util.control.Breaks._


trait NonCaseClassReflection:
  // self: TastyReflection =>

  def inspectNonCaseClass(reflect: Reflection)(
    symbol:                reflect.Symbol,
    tob:                   List[reflect.Type],
    paramSymbols:          Array[TypeSymbol],
    classDef:              reflect.ClassDef,
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
    import reflect.{_, given _}

    var index: Int = fields.length - 1

    val fieldNames = fields.map(_.name)

    val varAnnos = scala.collection.mutable.Map.empty[String,Map[String, Map[String,String]]]
    val varDefDeclarations = classDef.body.collect{
        // We just want public var definitions here
        case s: ValDef if !s.symbol.flags.is(reflect.Flags.Private) 
          && !s.symbol.flags.is(reflect.Flags.Protected) 
          && !fieldNames.contains(s.name) 
          && s.symbol.flags.is(reflect.Flags.Mutable) => 
            val annoSymbol = s.symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
            val fieldAnnos = 
              annoSymbol.map{ a => 
                val reflect.Apply(_, params) = a
                val annoName = a.symbol.signature.resultSig
                (annoName, annoSymToString(reflect)(params))

              }.toMap
            varAnnos.put(s.name, fieldAnnos) // yes, this is a side-effect but it saves mutliple field scans!
            s.name -> s.tpt.tpe.asInstanceOf[reflect.TypeRef]

        // We just want public def definitions here
        // WARNING: These defs may also include non-field functions!  Filter later...
        case d: DefDef if !d.symbol.flags.is(reflect.Flags.Private) 
          && !d.symbol.flags.is(reflect.Flags.Protected) 
          && !d.name.endsWith("_=") => d.name -> d.returnTpt.tpe.asInstanceOf[reflect.TypeRef]
    }.toMap

    val numConstructorFields = fields.length

    // Include inherited methods (var & def), including inherited!
    val baseAnnos = superClass match {
      case Some(c: ScalaClassInfo) => c.nonConstructorFields.map( f => f.name -> (f.annotations - S3ANNO) ).toMap
      case _ => Map.empty[String,Map[String, Map[String,String]]]
    }

    // val seen = scala.collection.mutable.Set.empty[Symbol]

    // FIX: Traverse up ALL the parents, until the first found fieldName is discovered... not just the first parent!
    def upTreeFind(fieldName: String, sym: Symbol): Option[Symbol] = 
      var foundSymbol: Option[Symbol] = None
      breakable {
        symbol.tree.asInstanceOf[ClassDef].parents.foreach{ _ match {
          case tt: TypeTree => //if !seen.contains(tt.tpe.classSymbol.get) =>
            // seen += tt.tpe.classSymbol.get
            tt.tpe.classSymbol.get.field(fieldName) match {
              case dotty.tools.dotc.core.Symbols.NoSymbol if tt.tpe.classSymbol.get.fullName == "java.lang.Object" => // top of tree 
              case dotty.tools.dotc.core.Symbols.NoSymbol => upTreeFind(fieldName, tt.tpe.classSymbol.get)
              case found => 
                foundSymbol = Some(found)
                break
            }
          case _ =>
        }}
      }
      foundSymbol


    // Include inherited methods (var & def), including inherited!
    // Produces (val <field>, method <field>_=)
    val getterSetter: List[(Symbol,Symbol)] = symbol.methods.filter(_.name.endsWith("_=")).map{ setter => 
      // Trying to get the setter... which could be a val (field) if declared is a var, or it could be a method 
      // in the case of user-written getter/setter... OR it could be defined in the superclass
      symbol.field(setter.name.dropRight(2)) match {
        case dotty.tools.dotc.core.Symbols.NoSymbol => 
          symbol.method(setter.name.dropRight(2)) match {
            case Nil => 
              upTreeFind(setter.name.dropRight(2), symbol) match {
                case Some(getter) => 
                  (getter, setter)
                case None =>
                  throw new ReflectException(s"Can't find field getter ${setter.name.dropRight(2)} in class ${symbol.fullName} or its superclass(es).")
              }
            case getter => 
              (getter.head, setter)
          }
        case getter: Symbol => 
          (getter, setter)
      }
    }

    val knownAnnos = baseAnnos ++ getterSetter.map{ (fGet, fSet) =>
      val both = fGet.annots ++ fSet.annots
      val annoMap = both.map{ a => 
        val reflect.Apply(_, params) = a
        val annoName = a.symbol.signature.resultSig
        (annoName, annoSymToString(reflect)(params))
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
        val tpe: Type = fGet.tree match {
          case vd: ValDef => vd.tpt.tpe
          case dd: DefDef => dd.returnTpt.tpe
        }
        typeSymbols.find(_ == tpe.typeSymbol.name)
      }

      val rtype = 
        originalTypeSymbol.map( ots => RType.unwindType(reflect)(tob(typeSymbols.indexOf(ots)).asInstanceOf[Type]) ).getOrElse{
          if varDefDeclarations.contains(fieldName) then
            RType.unwindType(reflect)(varDefDeclarations(fieldName))
          else
            fGet.tree match {
              case dd: DefDef => RType.unwindType(reflect)(dd.returnTpt.tpe)
              case vd: ValDef => RType.unwindType(reflect)(vd.tpt.tpe)
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
