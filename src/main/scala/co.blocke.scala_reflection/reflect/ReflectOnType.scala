package co.blocke.scala_reflection
package reflect

import scala.quoted.*
import rtypes.*

/**
  * ReflectOnType in the initial point of entry and triage when we get a Type to reflect on.  Most of the time this Type
  * will be some class, but we can't assume that.  Intersection and Union types are not classes and have no classSymbol.
  * Even for "normal" classes we want to collect some initial information before diving into the class itself.  The goal
  * is to smooth out some of the "-isms" and oddments before reflecing on the class, to make the job a little easier.
  */
object ReflectOnType: // extends NonCaseClassReflection:

  def apply[T](quotes: Quotes)(aType: quotes.reflect.TypeRepr, typedName: TypedName, resolveTypeSyms: Boolean): RType[T] = 
    import quotes.reflect.*

    val typeRef = aType.asInstanceOf[TypeRef]
    typeRef.classSymbol match {

      case None => 
        typeRef match {
          // Intersection Type
          //----------------------------------------
          case AndType(left,right) =>
            val resolvedLeft = RType.unwindType(quotes)(left.asInstanceOf[quotes.reflect.TypeRef])
            val resolvedRight = RType.unwindType(quotes)(right.asInstanceOf[quotes.reflect.TypeRef])
            val typeSymbols = List("L","R")
            IntersectionRType(Clazzes.INTERSECTION_CLASS, typeSymbols, resolvedLeft, resolvedRight)      

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft = RType.unwindType(quotes)(left.asInstanceOf[quotes.reflect.TypeRef])
            val resolvedRight = RType.unwindType(quotes)(right.asInstanceOf[quotes.reflect.TypeRef])
            val typeSymbols = List("L","R")
            UnionRType(Clazzes.UNION_CLASS, typeSymbols, resolvedLeft, resolvedRight)
        }

      // Most types will have a classSymbol and will be handled here...
      case Some(classSymbol) =>
        // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
        val (is2xEnumeration, className) = classSymbol.fullName match {           
          case raw if raw == Clazzes.ENUMERATION_CLASS => 
            val enumerationClass = typeRef.typeSymbol.fullName
            if( enumerationClass == Clazzes.ENUMERATION_CLASS ) then
              // If caller did NOT define a type member (type X = Value) inside their Enumeration class
              val enumClassName = typeRef.qualifier.asInstanceOf[quotes.reflect.TermRef].termSymbol.moduleClass.fullName.dropRight(1) // chop the '$' off the end!
              (true, enumClassName)
            else
              // If caller defined a type member (type X = Value) inside their Enumeration class
              (true, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
          case _  => 
            (false, classSymbol.fullName)
        }

        typeRef match {
          // Scala3 opaque type alias
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType if classSymbol == Symbol.classSymbol("scala.Any") && typeRef.isOpaqueAlias =>
            val translucentSuperType = typeRef.translucentSuperType
            val wrappedType = RType.unwindType(quotes)(translucentSuperType)
            AliasRType(typeRef.show, wrappedType).asInstanceOf[RType[T]]

          // Scala3 Tasty-equipped type incl. primitive types
          // Traits and classes w/type parameters are *not* here... they're AppliedTypes
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType => 
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
            classSymbol match {

              case cs if isTypeParam => 
                TypeSymbolRType(typeRef.name)  // TypeSymbols Foo[T] have typeRef of Any

              case cs if is2xEnumeration => 
                val enumerationClassSymbol = typeRef.qualifier.termSymbol.moduleClass
                ScalaEnumerationRType(
                  className, 
                  enumerationClassSymbol.declaredFields.map( _.name )
                  )

              case a if a == defn.AnyClass =>
                AnyRType().asInstanceOf[RType[T]]  // Any type

              case cs => // Non-parameterized classes
                ReflectOnClass(quotes)(typeRef, typedName, resolveTypeSyms)
            }

          // Union Type (sometimes it pops up down here for some reason... hmm...)
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft = RType.unwindType(quotes)(left.asInstanceOf[quotes.reflect.TypeRef])
            val resolvedRight = RType.unwindType(quotes)(right.asInstanceOf[quotes.reflect.TypeRef])
            val typeSymbols = List("L","R")
            UnionRType(Clazzes.UNION_CLASS, typeSymbols, resolvedLeft, resolvedRight)

          // Parameterized Types (classes, traits, & collections)
          //----------------------------------------
          case a @ AppliedType(t,tob) => 
            // First see if we have some sort of collection or other "wrapped" type
            val foundType: Option[RType[_]] = ExtractorRegistry.extractors.collectFirst {
              case e if e.matches(quotes)(classSymbol) => 
                e.extractInfo(quotes)(t, tob, classSymbol)
            }
            foundType.getOrElse {
              // Nope--we've got a parameterized class or trait here
              ReflectOnClass(quotes)(a.asInstanceOf[TypeRef], RType.typeName(quotes)(a), resolveTypeSyms, tob)
            }.asInstanceOf[RType[T]]
        
          case _ => // === No idea!  Unknown entity...
            UnknownRType(className)
        }
      }