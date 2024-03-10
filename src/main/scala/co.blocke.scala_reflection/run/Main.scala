package co.blocke.scala_reflection
package run

import neotype.*
import rtypes.*

object Main {

  def main(args: Array[String]): Unit =
    println(RType.of[Food].pretty)
    println("done")
}
