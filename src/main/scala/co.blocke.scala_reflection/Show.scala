package co.blocke.scala_reflection

import rtypes.*
import java.lang.StringBuilder

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
            .replaceAll("scala.","")
            .replaceAll("java.lang.","")
            .replaceAll("scala.util.","")
            .replaceAll("java.util.","")

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
                buf.append(showSimpleName(t)+" of ")
                val (_, lastWasMultiLine, classesSeenBefore) = _show(t.optionParamType, buf, tabLevel, seenBefore)
                (buf, lastWasMultiLine, classesSeenBefore)
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
