package co.blocke.scala_reflection

import munit.*
import co.blocke.reflect.{ClassAnno,FieldAnno}
import info.*
import impl.PrimitiveType.*

//
//inline def describe(message: String, color: String = Console.MAGENTA): Unit = println(s"$color$message${Console.RESET}")
//inline def pending = describe("   << Test Pending (below) >>", Console.YELLOW)

@Skip_Reflection
trait Nope:
  val extra: Int

@Skip_Reflection
class Ha( greg: Set[(String,List[Int])], graham: Nope, val garth: Boolean)

class Simple extends munit.FunSuite:

  test("non val constructor args") {
    val result = RType.of[Ha]
    println(result)
  }

