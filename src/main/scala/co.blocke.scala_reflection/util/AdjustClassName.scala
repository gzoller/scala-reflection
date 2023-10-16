package co.blocke.scala_reflection
package util

object AdjustClassName:

  /** @param name name of class
    * @return adjusted name - one that is converted to a class name that works with Java Reflection
    */
  def apply(originalName: String): String =
    val splitPos = originalName.indexOf("$.")
    if splitPos == -1 then originalName
    else
      val prefix = originalName.substring(0, splitPos)
      val suffix = originalName.substring(splitPos + 1)
      new StringBuilder(originalName.length)
        .append(prefix)
        .append(suffix.replace(".", "$"))
        .toString()
