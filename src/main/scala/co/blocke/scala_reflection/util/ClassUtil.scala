package co.blocke.scala_reflection.util

object ClassUtil:
  /**
   * @param name name of class
   * @return adjusted name - one that is converted to a class name that works with Java Reflection
   */
  def adjustClassName(name: String): String =
    val splitPos = name.indexOf("$.")
    if (splitPos == -1) {
      name
    } else {
      val prefix = name.substring(0, splitPos)
      val suffix = name.substring(splitPos + 1)
      val s = new StringBuilder(name.length)
        .append(prefix)
        .append(suffix.replace(".", "$"))
        .toString()
      println(">>>>> adjusted " + s + " - input " + name)
      s
    }
