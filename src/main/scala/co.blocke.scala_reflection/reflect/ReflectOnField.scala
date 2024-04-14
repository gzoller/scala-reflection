package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import rtypeRefs.*

object ReflectOnField:

  // Scala case class field reflection
  def apply(quotes: Quotes)(
      fieldType: RTypeRef[?],
      valDef: quotes.reflect.ValDef,
      index: Int,
      dad: Option[ClassRef[_]],
      fieldDefaultMethods: Map[Int, (String, String)],
      isNonValConstructorField: Boolean = false
  ): FieldInfoRef =
    import quotes.reflect.*

    // Get any field annotations (from body of class--they're not on the constructor fields)
    val fieldAnnos = dad match {
      case _ =>
        val baseAnnos = dad
          .flatMap(_.fields.find(_.name == valDef.name))
          .map(_.annotations)
          .getOrElse(Map.empty[String, Map[String, String]])
        baseAnnos ++ valDef.symbol.annotations.map { a =>
          val Apply(_, params) = a: @unchecked
          val annoName = a.symbol.signature.resultSig
          (annoName, annoSymToString(quotes)(params))
        }.toMap
    }

    // Figure out the original type symbols, i.e. T, (if any)
    val valTypeRef = valDef.tpt.tpe.asInstanceOf[quotes.reflect.TypeRef]
    val isTypeParam = valTypeRef.typeSymbol.flags.is(quotes.reflect.Flags.Param)
    val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None

    ScalaFieldInfoRef(
      index,
      valDef.name,
      fieldType,
      fieldAnnos,
      fieldDefaultMethods.get(index),
      originalTypeSymbol,
      isNonValConstructorField
    )(using quotes)

  // Reflect on any fields in a Scala plain class (non-case class) that are NOT in the constructor, i.e. defined in the body
  def nonCaseScalaField[Q](
      quotes: Quotes
  )(symbol: quotes.reflect.Symbol, classRepr: quotes.reflect.TypeRepr, constructorFieldNames: List[String])(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): List[NonConstructorFieldInfoRef] =
    import quotes.reflect.*

    // Include inherited methods (var & def), including inherited!
    // Produces (val <field>, method <field>_=)
    symbol.methodMembers
      .filter(_.name.endsWith("_="))
      .filterNot(f => constructorFieldNames.contains(f.name.dropRight(2))) // filter out any 'var' constructor fields that slip through...
      .map { setter =>
        // Trying to get the setter... which could be a val (field) if declared is a var, or it could be a method
        // in the case of user-written getter/setter... OR it could be defined in the superclass
        symbol.fieldMember(setter.name.dropRight(2)) match {
          case dotty.tools.dotc.core.Symbols.NoSymbol =>
            symbol.methodMember(setter.name.dropRight(2)) match {
              case Nil =>
                throw new ReflectException(
                  s"Can't find field getter ${setter.name.dropRight(2)} in class ${symbol.fullName} or its superclass(es)."
                )
              case getter =>
                (getter.head, setter)
            }
          case getter: Symbol =>
            (getter, setter)
        }
      }
      .filterNot { (getterSym, setterSym) =>
        getterSym.annotations.map(_.tpe.typeSymbol.fullName).contains("co.blocke.scala_reflection.Ignore") ||
        setterSym.annotations.map(_.tpe.typeSymbol.fullName).contains("co.blocke.scala_reflection.Ignore")
      }
      .zipWithIndex
      .map { case ((getter, setter), i) =>
        // Pull out field annotations (either getter or setter can be annotated)
        val varAnnos = (getter.annotations ++: setter.annotations).map { a =>
          val Apply(_, params) = a: @unchecked
          (a.symbol.signature.resultSig, annoSymToString(quotes)(params))
        }.toMap

        NonConstructorFieldInfoRef(
          i,
          getter.name, // field name is same as getter method name
          getter.name,
          setter.name,
          getter.isValDef,
          applyTypeToField(quotes)(getter, classRepr),
          varAnnos
        )(using quotes)
      }
      .sortBy(_.getterLabel) // sorted for consistent ordering for testing ;-)

  // Reflect on any fields in a Scala plain class (non-case class) that are NOT in the constructor, i.e. defined in the body
  def javaFields[Q](
      quotes: Quotes
  )(symbol: quotes.reflect.Symbol, classRepr: quotes.reflect.TypeRepr)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): List[NonConstructorFieldInfoRef] =
    import quotes.reflect.*

    val allMethods = symbol.methodMembers
    val getterMethods = symbol.methodMembers.filter(_.name.startsWith("get"))
    val setterMethods = getterMethods.map(g => symbol.methodMember("set" + g.name.drop(3)).headOption)
    getterMethods
      .zip(setterMethods)
      .collect {
        case (g, s) if s.isDefined =>
          val chars = g.name.drop(3).toCharArray()
          chars(0) = chars(0).toLower
          (String(chars), g, s.get)
      }
      .filterNot { (_, getterSym, setterSym) =>
        getterSym.annotations.map(_.tpe.typeSymbol.fullName).contains("co.blocke.scala_reflection.Ignore") ||
        setterSym.annotations.map(_.tpe.typeSymbol.fullName).contains("co.blocke.scala_reflection.Ignore")
      }
      .zipWithIndex
      .map { case ((fieldName, getter, setter), i) =>
        val fieldType = setter.tree match {
          case dd: DefDef =>
            dd.paramss.head.params.head match {
              case v: ValDef  => applyTypeToField(quotes)(getter, classRepr)
              case t: TypeDef => UnknownRef[Q]("Where do we go from here?")(using quotes)(using classRepr.asType.asInstanceOf[Type[Q]])
            }
        }
        val varAnnos = (getter.annotations ++: setter.annotations).map { a =>
          val Apply(_, params) = a: @unchecked
          (a.symbol.signature.resultSig, annoSymToString(quotes)(params))
        }.toMap
        NonConstructorFieldInfoRef(
          i,
          fieldName,
          getter.name,
          setter.name,
          false,
          fieldType,
          varAnnos,
          None
        )(using quotes)
      }
      .sortBy(_.getterLabel) // sorted for consistent ordering for testing ;-)

  def applyTypeToField(quotes: Quotes)(symbol: quotes.reflect.Symbol, classRepr: quotes.reflect.TypeRepr)(using seenBefore: scala.collection.mutable.Map[TypedName, Boolean]): RTypeRef[?] =
    import quotes.reflect.*

    implicit val q = quotes
    symbol.tree match {
      case v: ValDef =>
        classRepr.memberType(symbol).asType match
          case '[t] =>
            ReflectOnType[t](quotes)(classRepr.memberType(symbol))
      case d: DefDef =>
        classRepr.memberType(symbol) match {
          case m: MethodType => // normal method w/parens
            m.resType.asType match
              case '[t] =>
                ReflectOnType[t](quotes)(m.resType)
          case b: ByNameType => // ExprType, which is a method (like a setter) with no parens
            b.underlying.asType match
              case '[t] =>
                ReflectOnType[t](quotes)(b.underlying)
        }
    }
