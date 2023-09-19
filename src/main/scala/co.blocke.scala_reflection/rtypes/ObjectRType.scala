package co.blocke.scala_reflection
package rtypes


case class ObjectRType(
    name: String
  ) extends RType[Object]:

  val typedName = name
  lazy val clazz: Class[_] = Class.forName(name)

  // def show(tab: Int = 0, seenBefore: List[String] = Nil, suppressIndent: Boolean = false, modified: Boolean = false): String = 
  //   {if(!suppressIndent) tabs(tab) else ""} + this.getClass.getSimpleName + s"($name)\n"
