package co.blocke.scala_reflection
package util

import scala.quoted.*
import rtypeRefs.FieldInfoRef

// Utility classes to help generate JSON from RTypes--used by ScalaJS

case class JsonField[J](name: String, value: J)(using t: Type[J]):
  type F = J
  val tt = t

// This is a limited JSON builder helper.  Limited because it's not general-purpose.  There are a lot
// of types that have no support here.  We only needed to support the subset of types that the various
// RType classes have as case fields.
// If you're tempted to lift this for use elsewhere... you have been warned!
// The cool thing is it uses quotes and reflection to discern the field types passed in.
object JsonObjectBuilder:

  def apply(quotes: Quotes)(sb: StringBuilder, items: List[JsonField[_]]): Unit =
    import quotes.reflect.*

    sb.append('{')
    items
      .map { one =>
        val repr = TypeRepr.of[one.F](using one.tt)
        (one, repr)
      }
      .map { (item, repr) =>
        sb.append(s"\"${item.name}\":")
        matchOneType(quotes)(sb, repr, item.value)
        sb.append(',')
      }
    sb.setCharAt(sb.length() - 1, '}')

  private def matchOneType(quotes: Quotes)(sb: StringBuilder, repr: quotes.reflect.TypeRepr, item: Any): Unit =
    import quotes.reflect.*
    println("JS: " + repr.typeSymbol.name + " >> " + item)
    repr.typeSymbol.name match {
      case "String" | "TypedName" | "TypeSymbol" => sb.append(s"\"${item.toString}\"")

      case "Boolean" | "Int" => sb.append(s"${item.toString}")

      case "List" =>
        sb.append('[')
        repr match {
          case AppliedType(t, tob) =>
            item.asInstanceOf[List[_]].map { listItem =>
              matchOneType(quotes)(sb, tob.head, listItem)
              sb.append(',')
            }
        }
        if item.asInstanceOf[List[_]].isEmpty then sb.append(']')
        else sb.setCharAt(sb.length() - 1, ']')

      case "Map" =>
        sb.append('{')
        repr match {
          case AppliedType(t, tob) =>
            item.asInstanceOf[Map[String, _]].map { (mapKey, mapValue) =>
              matchOneType(quotes)(sb, tob(0), mapKey)
              sb.append(':')
              matchOneType(quotes)(sb, tob(1), mapValue)
              sb.append(',')
            }
        }
        if item.asInstanceOf[Map[String, _]].isEmpty then sb.append('}')
        else sb.setCharAt(sb.length() - 1, '}')

      case "Option" =>
        if item.asInstanceOf[Option[_]].isEmpty then sb.append("null")
        else
          repr match {
            case AppliedType(t, tob) =>
              matchOneType(quotes)(sb, tob.head, item.asInstanceOf[Option[_]].get)
          }

      case "FieldInfoRef" | "NonConstructorFieldInfoRef" | "ScalaFieldInfoRef" =>
        item.asInstanceOf[FieldInfoRef].asJson(sb)(using quotes)

      case _ => // Careful!  This is a bold assumption that everything else is RTypeRef[_]!
        item.asInstanceOf[RTypeRef[_]].asJson(sb)(using quotes)
    }
