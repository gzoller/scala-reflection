package co.blocke.scala_reflection

import rtypes.*
import java.lang.StringBuilder
import scala.util.matching.Regex

// Pretty-print complex structures of RTypes
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
        rt.name
            .replaceAll("java.lang.","")
            .replaceAll("java.util.","")
            .replaceAll("scala.util.","")
            .replaceAll("scala.","")

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

            case t: TypeSymbolRType =>
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
                (buf.append(showSimpleName(t)+ " (recursive self-reference)"), false, seenBefore)

            case t: ScalaClassRType[_] =>
                if seenBefore.contains(t.typedName.toString) then
                    buf.append(t.name + " (seen before, details above)\n")
                    (buf, true, seenBefore)
                else
                    buf.append(t.name)
                    if t.paramSymbols.nonEmpty then
                        buf.append( t.paramSymbols.map(_.toString).mkString("[",",","]"))
                    buf.append(":\n")
                    buf.append(tabs(tabLevel+1))
                    buf.append("fields ->\n")
                    val allClassesSeenUpToNow = t.fields.foldLeft(t.typedName.toString :: seenBefore){ (classesSeen, f) =>
                        buf.append(tabs(tabLevel+2))
                        buf.append(f.name+": ")
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
                        classesSeenBefore
                    }
                    (buf, true, allClassesSeenUpToNow)
        }
