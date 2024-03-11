package co.blocke.scala_reflection
package run

import model.*
object Main {

  def main(args: Array[String]): Unit =
    println(RType.of[NeoPerson].pretty)
    println("done")
}
