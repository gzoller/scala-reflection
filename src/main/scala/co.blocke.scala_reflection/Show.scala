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

    private def _show(rt: RType[_], buf: StringBuilder, tabLevel: Int = 0): (StringBuilder, Boolean) = 
        rt match {
            case t: PrimitiveRType => 
                (buf.append(lastPart(t.name)),false)
            case t: TypeSymbolRType =>
                (buf.append(t.name),false)
            case t: TypeMemberRType =>
                (buf.append(t.name),false)
            case t: OptionRType[_] =>
                buf.append(showSimpleName(t)+" of ")
                val (_, lastWasMultiLine) = _show(t.optionParamType, buf, tabLevel)
                (buf,lastWasMultiLine)
            case t: ScalaClassRType[_] =>
                buf.append(t.name + ":\n")
                buf.append(tabs(tabLevel+1))
                buf.append("fields ->\n")
                t.fields.map{f =>
                    buf.append(tabs(tabLevel+2))
                    buf.append(f.name+": ")
                    val (_, lastWasMultiLine) = _show(f.fieldType, buf, tabLevel+2)
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
                }
                (buf,true)
        }
