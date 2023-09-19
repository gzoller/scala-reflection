package co.blocke.scala_reflection

import munit.*
import models.*

class Basic extends munit.FunSuite:

  test("Class of all primitives") {
    val result = RType.of[Prim]
    assertEquals(result.pretty(), """co.blocke.scala_reflection.models.Prim:
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
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.Person:
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
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.HasDefaults:
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
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.SelfReferencing:
        |   fields ->
        |      a: String
        |      b: co.blocke.reflection.models.SelfReferencing (recursive self-reference)
        |      c: Int
        |      d: Option of co.blocke.reflection.models.SelfReferencing (recursive self-reference)
        |""".stripMargin)
    assertEquals(result.clazz.getName, "co.blocke.scala_reflection.models.SelfReferencing")    
  }

  test("sealed trait with case classes") {
    val result = RType.of[VehicleHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.VehicleHolder:
        |   fields ->
        |      v: co.blocke.reflection.models.Vehicle (sealed trait):
        |         children ->
        |            co.blocke.scala_reflection.models.Truck:
        |               fields ->
        |                  numberOfWheels: Int
        |            co.blocke.scala_reflection.models.Car:
        |               fields ->
        |                  numberOfWheels: Int
        |                  color: String
        |            co.blocke.scala_reflection.models.Plane:
        |               fields ->
        |                  numberOfEngines: Int
        |""".stripMargin)
  }

  test("sealed trait with case objects") {
    val result = RType.of[FlavorHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.FlavorHolder:
        |   fields ->
        |      f: co.blocke.reflection.models.Flavor (sealed trait):
        |         children ->
        |            co.blocke.reflection.models.Vanilla (object)
        |            co.blocke.reflection.models.Chocolate (object)
        |            co.blocke.reflection.models.Bourbon (object)
        |""".stripMargin)
  }
