package co.blocke.scala_reflection
package rtypes

/** RType for unassigned type symbol, e.g. Foo[T]
 */

case class TypeSymbolRType(name: String) extends RType[Any]
//   def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String =
//     {if(!suppressIndent) tabs(tab) else ""} + name + "\n"
