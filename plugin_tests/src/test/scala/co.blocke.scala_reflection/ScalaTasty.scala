package co.blocke.scala_reflection

import munit.*
import info.*
import impl.PrimitiveType.*
import com.fasterxml.jackson.databind.json.JsonMapper

case class Person(name: String, age: Int, other: Int | Boolean)
class Mixin (mapper: JsonMapper)

object Outer:
  final class Mixin2 private[Outer] (nada: String)

inline def describe(message: String, color: String = Console.MAGENTA): Unit = println(s"$color$message${Console.RESET}")
inline def pending = describe("   << Test Pending (below) >>", Console.YELLOW)

class ScalaTasty extends munit.FunSuite:

  test("reflect basic Tasty class with union") {
    val result = RType.of[Person]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Person):
                                   |   fields:
                                   |      (0) name: java.lang.String
                                   |      (1) age: scala.Int
                                   |      (2) other: Union:
                                   |         left--scala.Int
                                   |         right--scala.Boolean
                                   |""".stripMargin)
  }

  test("plugin-created annotation is present") {
    val rt = RType.of[Person]
    assert(rt.infoClass.getAnnotations.toList.collectFirst{
      case s3r: S3Reflection => true
    }.getOrElse(false))
  }

  test("fancy annotation test") {
    val rt = RType.of[Mixin] // RType.of[Outer.Mixin2]  // <-- This works, so the final/private stuff isn't causing the problem
    assert(rt.infoClass.getAnnotations.toList.collectFirst{
      case s3r: S3Reflection => true
    }.getOrElse(false))
    println(rt)
  }
