package co.blocke.scala_reflection.impl

import java.io.File
import java.nio.file.{Path, Paths}

private[scala_reflection] object PathResolver {
   def findPathForClass(clazz: Class[?]): Option[Path] = {
    Option(clazz.getProtectionDomain.getCodeSource)
      .map(src => src.getLocation.toURI)
      .map(src => Paths.get(src))
      .map(path => path.resolve(clazz.getName.replace(".", File.separator) + ".tasty").normalize())
  }

}
