package co.blocke.scala_reflection
package run

import model.*
object Main {

  def main(args: Array[String]): Unit =
    println(RType.of[Schema].pretty)
    println("done")
}
