package co.blocke.scala_reflection

import rtypes.*
import java.lang.StringBuilder
import scala.util.matching.Regex

// Pretty-print complex structures of RTypes.
// Have this all in one object vs in the RTypes, because this is a "luxery" add--not intrinsic to the operation of
// the RType, and I wanted to keep the RType small.
object Show:

    def show( rt: RType[_] ): String =
        val buf = new StringBuilder()
        _show(rt, buf)._1.toString

    final inline def tabs(t:Int) = "   "*t
    final inline def lastPart(n: String) = n.split('.').last
    final inline def clipDefault(v: Any) = {
        val s = v.toString
        if s.length > 20 then
            s.take(20)+"..."
        else
            s
    }
    final inline def showSimpleName(rt: RType[_]): String =
        if rt.name.startsWith("scala.") || rt.name.startsWith("java.") then
            rt.name.split('.').last
        else
            rt.name

    def typeString(rtypes: List[RType[_]]): String = 
        val buf = new StringBuilder("[")
        rtypes.map{ rt => 
            _typeString(rt, buf) 
            buf.append(",")
        }
        buf.setCharAt(buf.length()-1,']')
        buf.toString

    inline def pureTypeName(in: String): String = in.split('.').last 

    private def _typeString(rt: RType[_], buf: StringBuilder): StringBuilder =
        rt match {
            case u: UnionRType[_] => 
                _typeString(u.leftType, buf)
                buf.append(" | ")
                _typeString(u.rightType, buf)

            case i: IntersectionRType[_] => 
                _typeString(i.leftType, buf)
                buf.append(" & ")
                _typeString(i.rightType, buf)

            case t: TupleRType[_] => 
                buf.append( "(")
                (0 to t.selectLimit-1).map{ i =>
                    _typeString(t.select(i), buf)
                    buf.append(",")
                    }
                buf.setCharAt(buf.length()-1,')')
                buf

            case a: AppliedRType =>  // parameterized thing
                buf.append( pureTypeName(a.name) )
                if a.typeParamSymbols.nonEmpty then 
                    buf.append("[")
                    (0 to a.selectLimit-1).map{ i =>
                        _typeString(a.select(i), buf)
                        buf.append(",")
                        }
                    buf.setCharAt(buf.length()-1,']')
                buf

            case _ => 
                buf.append(pureTypeName(rt.name))
        }

    val normalColl: Regex = """^scala.collection.immutable.(\w+)$""".r
    val mutableColl: Regex = """^scala.collection.mutable.(\w+)$""".r
    final inline def cleanCollectionNames(rt: RType[_]): String =
        rt.name match {
            case normalColl(c) => c
            case mutableColl(c) => "mutable " + c
        }

    inline def showOfType(buf: StringBuilder, seenBefore: List[String], tabLevel: Int, label: String, rt: RType[_]) =
        buf.append(label)
        val (_, lastWasMultiLine, classesSeenBefore) = _show(rt, buf, tabLevel, seenBefore)
        (buf, lastWasMultiLine, classesSeenBefore)

    private def _show(
        rt: RType[_], 
        buf: StringBuilder, 
        tabLevel: Int = 0, 
        seenBefore: List[String] = List.empty[String]  // classes we've seen before (not primitives)
        ): (StringBuilder, Boolean, List[String]) = 
        rt match {
            case t: PrimitiveRType => 
                (buf.append(lastPart(t.name)), false, seenBefore)

            case t: TypeSymbolRType[_] =>
                (buf.append(t.name), false, seenBefore)

            case t: TypeMemberRType =>
                (buf.append(t.name), false, seenBefore)

            case t: OptionRType[_] =>
                showOfType(buf, seenBefore, tabLevel, showSimpleName(t)+" of ", t.optionParamType)

            case t: TryRType[_] =>
                showOfType(buf, seenBefore, tabLevel, "Try of ", t.tryType)

            case t: SeqRType[_] =>
                showOfType(buf, seenBefore, tabLevel, cleanCollectionNames(t) + " of: ", t.elementType)

            case t: ArrayRType[_] =>
                showOfType(buf, seenBefore, tabLevel, "Array of: ", t.elementType)

            case t: TupleRType[_] =>
                buf.append("Tuple of:\n")
                val allClassesSeenUpToNow = t._tupleTypes.zipWithIndex.foldLeft(seenBefore){ case (classesSeen, (rt, i)) =>
                    buf.append(tabs(tabLevel+1))
                    buf.append(s"$i: ")
                    val (_, lastWasMultiLine, classesSeenBefore) = _show(rt, buf, tabLevel+1, classesSeen)
                    if !lastWasMultiLine then
                        buf.append("\n")
                    classesSeenBefore
                }
                (buf, true, allClassesSeenUpToNow)

            case t: MapRType[_] =>
                buf.append(cleanCollectionNames(t)+" of:\n")
                buf.append(tabs(tabLevel+1))
                buf.append("key: ")
                val (_, lastWasMultiLine_1, classesSeen_1) = _show(t._elementType, buf, tabLevel+1, seenBefore)
                if !lastWasMultiLine_1 then
                    buf.append("\n")
                buf.append(tabs(tabLevel+1))
                buf.append("value: ")
                val (_, lastWasMultiLine_2, classesSeen_2) = _show(t._elementType2, buf, tabLevel+1, classesSeen_1)
                if !lastWasMultiLine_2 then
                    buf.append("\n")
                (buf, true, classesSeen_2)

            case t: LeftRightRType[_] =>
                buf.append(showSimpleName(t)+" of:\n")
                buf.append(tabs(tabLevel+1))
                buf.append("left--")
                val (_, lastWasMultiLine, classesSeenBefore1) = _show(t.leftType, buf, tabLevel+2, seenBefore)
                if !lastWasMultiLine then
                    buf.append("\n")
                buf.append(tabs(tabLevel+1))
                buf.append("right--")
                val (_, lastWasMultiLine2, classesSeenBefore2) = _show(t.rightType, buf, tabLevel+2, classesSeenBefore1)
                if !lastWasMultiLine2 then
                    buf.append("\n")
                (buf, true, classesSeenBefore2)

            case t: SelfRefRType[_] =>
                (buf.append(t.name + " (recursive self-reference)"), false, seenBefore)

            case t: ScalaClassRType[_] =>
                if seenBefore.contains(t.typedName.toString) then
                    buf.append(t.name + " (seen before, details above)\n")
                    (buf, true, seenBefore)
                else
                    buf.append(t.name)
                    if t.typeParamValues.nonEmpty then
                        buf.append(typeString(t.typeParamValues))
                    else if t.typeParamSymbols.nonEmpty then
                        buf.append( t.typeParamSymbols.map(_.toString).mkString("[",",","]"))
                    if t.isValueClass then
                        buf.append(" (value class)")
                    buf.append(":\n")
                    buf.append(tabs(tabLevel+1))
                    buf.append("fields ->\n")
                    val allClassesSeenUpToNow = t.fields.foldLeft(t.typedName.toString :: seenBefore){ (classesSeen, f) =>
                        buf.append(tabs(tabLevel+2))
                        buf.append(f.name)
                        if f.asInstanceOf[ScalaFieldInfo].isNonValConstructorField then
                            buf.append(" (set-only)")
                        buf.append(": ")
                        f.originalSymbol.map( os => buf.append("["+os+"] ") )
                        val (_, lastWasMultiLine, classesSeenBefore) = _show(f.fieldType, buf, tabLevel+2, classesSeen)
                        f.defaultValue.map{ default => 
                            if lastWasMultiLine then
                                buf.append(tabs(tabLevel+3))
                            else
                                buf.append(" ")
                            buf.append("(default value: "+clipDefault(default)+")")
                            if lastWasMultiLine then
                                buf.append("\n")
                        }
                        if !lastWasMultiLine then
                            buf.append("\n")
                        if f.annotations.nonEmpty then
                            buf.append(tabs(tabLevel+3))
                            buf.append("annotations -> " + f.annotations.toString + "\n")
                        classesSeenBefore
                    }
                    val allClassesSeenUpToNow2 = if t.nonConstructorFields.nonEmpty then
                        buf.append(tabs(tabLevel+1))
                        buf.append("non-constructor fields (non-case class) ->\n")
                        t.nonConstructorFields.foldLeft(t.typedName.toString :: seenBefore){ (classesSeen, f) => 
                            buf.append(tabs(tabLevel+2))
                            buf.append(f.getterLabel+": ")
                            val (_, lastWasMultiLine, classesSeenBefore) = _show(f.fieldType, buf, tabLevel+2, classesSeen)
                            // f.defaultValue.map{ default => 
                            //     if lastWasMultiLine then
                            //         buf.append(tabs(tabLevel+3))
                            //     else
                            //         buf.append(" ")
                            //     buf.append("(default value: "+clipDefault(default)+")")
                            //     if lastWasMultiLine then
                            //         buf.append("\n")
                            // }
                            if !lastWasMultiLine then
                                buf.append("\n")
                            if f.annotations.nonEmpty then
                                buf.append(tabs(tabLevel+3))
                                buf.append("annotations -> " + f.annotations.toString + "\n")
                            classesSeenBefore
                        }
                    else
                        allClassesSeenUpToNow

                    val allClassesSeenUpToNow3 = if t._typeMembers.nonEmpty then
                        buf.append(tabs(tabLevel+1))
                        buf.append("type members ->\n")
                        t._typeMembers.foldLeft(allClassesSeenUpToNow2){ (classesSeen, f) =>
                            buf.append(tabs(tabLevel+2))
                            buf.append(f.name+": ")
                            buf.append("["+f.typeSymbol+"] " )
                            val (_, lastWasMultiLine, classesSeenBefore) = _show(f.memberType, buf, tabLevel+2, classesSeen)
                            if !lastWasMultiLine then
                                buf.append("\n")
                            classesSeenBefore
                        }
                    else
                        allClassesSeenUpToNow2

                    if t.annotations.nonEmpty then
                        buf.append(tabs(tabLevel+1))
                        buf.append("annotations ->\n")
                        buf.append(tabs(tabLevel+2))
                        buf.append(t.annotations.toString+"\n")
                        
                    (buf, true, allClassesSeenUpToNow3)


            case t: TraitRType[_] =>
                if seenBefore.contains(t.typedName.toString) then
                    buf.append(showSimpleName(t) + " (seen before, details above)\n")
                    (buf, true, seenBefore)
                else
                    buf.append(showSimpleName(t))
                    if t.typeParamValues.nonEmpty then
                        buf.append(typeString(t.typeParamValues))
                    else if t.typeParamSymbols.nonEmpty then
                        buf.append( t.typeParamSymbols.map(_.toString).mkString("[",",","]"))
                    buf.append(" (trait):\n")
                    buf.append(tabs(tabLevel+1))
                    buf.append("fields ->\n")
                    val allClassesSeenUpToNow = t.fields.foldLeft(t.typedName.toString :: seenBefore){ (classesSeen, f) =>
                        buf.append(tabs(tabLevel+2))
                        buf.append(f.name+": ")
                        f.originalSymbol.map( os => buf.append("["+os+"] ") )
                        val (_, lastWasMultiLine, classesSeenBefore) = _show(f.fieldType, buf, tabLevel+2, seenBefore)
                        if !lastWasMultiLine then
                            buf.append("\n")
                        classesSeenBefore
                    }
                    (buf, true, allClassesSeenUpToNow)

            case t: SealedTraitRType[_] =>
                if seenBefore.contains(t.typedName.toString) then
                    buf.append(showSimpleName(t) + " (seen before, details above)\n")
                    (buf, true, seenBefore)
                else
                    buf.append(showSimpleName(t))
                    buf.append(" (sealed trait):\n")
                    buf.append(tabs(tabLevel+1))
                    buf.append("children ->\n")
                    val allClassesSeenUpToNow = t.children.foldLeft(t.typedName.toString :: seenBefore){ (classesSeen, f) =>
                        buf.append(tabs(tabLevel+2))
                        val (_, lastWasMultiLine, classesSeenBefore) = _show(f, buf, tabLevel+2, classesSeen)
                        if !lastWasMultiLine then
                            buf.append("\n")
                        classesSeenBefore
                    }
                    (buf, true, allClassesSeenUpToNow)

            case t: ScalaEnumerationRType[_] =>
                buf.append("Enumeration (Scala 2) having values "+t.values.mkString("(",",",")"))
                (buf, false, seenBefore)

            case t: ScalaEnumRType[_] =>
                buf.append("Enum (Scala 3) having values "+t.values.mkString("(",",",")"))
                (buf, false, seenBefore)

            case t: ObjectRType => 
                buf.append(showSimpleName(t))
                buf.append(" (object)")
                (buf, false, seenBefore)

            case t: UnknownRType[_] =>
                buf.append("unknown type: "+t.name)
                (buf, false, seenBefore)

            case t: AliasRType[_] =>
                showOfType(buf, seenBefore, tabLevel, s"alias ${lastPart(t.definedType)} defined as ", t.unwrappedType)

            case t: Scala2RType[_] =>
                buf.append(t.name+ " (Scala 2)")
                (buf, false, seenBefore)
        }
