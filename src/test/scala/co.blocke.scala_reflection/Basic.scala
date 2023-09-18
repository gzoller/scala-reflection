package co.blocke.scala_reflection

import munit.*
import models.*

class Basic extends munit.FunSuite:

  test("Class of all primitives") {
    val result = RType.of[Prim]
    println(result.prettyPrint().replaceAll("\n",">"))
    assertEquals(result.prettyPrint(), """co.blocke.scala_reflection.models.Prim:
        |   fields ->
        |      a: Boolean
        |      b: Byte
        |      c: Char
        |      d: Double
        |      e: Float
        |      f: Int
        |      g: Long
        |      h: Short
        |      i: String
        |      j: Any
        |""".stripMargin)
    assertEquals(result.clazz.getName, "co.blocke.scala_reflection.models.Prim")
  }

  test("Simple class having nested class as a parameter") {
    val result = RType.of[Person]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.Person:
        |   fields ->
        |      name: String
        |      age: Int
        |      item: co.blocke.scala_reflection.models.Item:
        |         fields ->
        |            desc: String
        |      allDone: Boolean
        |""".stripMargin)
    assertEquals(result.clazz.getName, "co.blocke.scala_reflection.models.Person")
  }

  test("Class having default values for its parameters") {
    val result = RType.of[HasDefaults]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.HasDefaults:
        |   fields ->
        |      a: String (default value: wow)
        |      item: co.blocke.scala_reflection.models.Item:
        |         fields ->
        |            desc: String
        |         (default value: Item(none))
        |      c: Int (default value: 5)
        |""".stripMargin)
    assertEquals(result.clazz.getName, "co.blocke.scala_reflection.models.HasDefaults")    
  }

  test("Class having self-referencing members") {
    val result = RType.of[SelfReferencing]  
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.SelfReferencing:
        |   fields ->
        |      a: String
        |      b: co.blocke.reflection.models.SelfReferencing (recursive self-reference)
        |      c: Int
        |      d: Option of co.blocke.reflection.models.SelfReferencing (recursive self-reference)
        |""".stripMargin)
    assertEquals(result.clazz.getName, "co.blocke.scala_reflection.models.SelfReferencing")    
  }
