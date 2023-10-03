package co.blocke.scala_reflection

import munit.*
import models.*

class Basic extends munit.FunSuite:

  test("Class of all primitives") {
    val result = RType.of[Prim]
    assertEquals(result.pretty, """co.blocke.scala_reflection.models.Prim:
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
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.Person:
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
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.HasDefaults:
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
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.SelfReferencing:
        |   fields ->
        |      a: String
        |      b: co.blocke.scala_reflection.models.SelfReferencing (recursive self-reference)
        |      c: Int
        |      d: Option of co.blocke.scala_reflection.models.SelfReferencing (recursive self-reference)
        |""".stripMargin)
    assertEquals(result.clazz.getName, "co.blocke.scala_reflection.models.SelfReferencing")    
  }

  test("sealed trait with case classes") {
    val result = RType.of[VehicleHolder]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.VehicleHolder:
        |   fields ->
        |      v: co.blocke.scala_reflection.models.Vehicle (sealed trait):
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
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.FlavorHolder:
        |   fields ->
        |      f: co.blocke.scala_reflection.models.Flavor (sealed trait):
        |         children ->
        |            co.blocke.scala_reflection.models.Vanilla (object)
        |            co.blocke.scala_reflection.models.Chocolate (object)
        |            co.blocke.scala_reflection.models.Bourbon (object)
        |""".stripMargin)
  }

  test("handle opaque type alias") {
    val result = RType.of[Employee]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.Employee:
        |   fields ->
        |      eId: alias EMP_ID defined as Int
        |      age: Int
        |""".stripMargin)
  }

  test("Scala 2.x class") {
    val result = RType.of[scala.math.BigDecimal]
    assertEquals( result.pretty, "scala.math.BigDecimal (Scala 2)")
  }

  test("support value classes") {
    val result = RType.of[Employee2]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.Employee2:
        |   fields ->
        |      eId: co.blocke.scala_reflection.models.IdUser (value class):
        |         fields ->
        |            id: Int
        |      age: Int
        |""".stripMargin)
  }

  test("Skip_Reflection annotation works") {
    val result = RType.of[SkipMe]
    assertEquals( result.pretty.stripLineEnd, """unknown type: co.blocke.scala_reflection.models.SkipMe""")
  }

  test("Self-referencing types (non-parameterized") {
    val result = RType.of[Shape]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.Shape:
        |   fields ->
        |      id: Int
        |      parent: Option of co.blocke.scala_reflection.models.Shape (recursive self-reference)
        |""".stripMargin)
    val result2 = RType.of[Person2]
    assertEquals( result2.pretty, """co.blocke.scala_reflection.models.Person2:
        |   fields ->
        |      name: String
        |      age: Int
        |      boss: co.blocke.scala_reflection.models.Person2 (recursive self-reference)
        |""".stripMargin)
  }

  test("Self-referencing types (parameterized") {
    val result = RType.of[Drawer[Shape]]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.Drawer[Shape]:
        |   fields ->
        |      id: Int
        |      nextInChain: Option of co.blocke.scala_reflection.models.Drawer (recursive self-reference)
        |      thing: [T] co.blocke.scala_reflection.models.Shape:
        |         fields ->
        |            id: Int
        |            parent: Option of co.blocke.scala_reflection.models.Shape (recursive self-reference)
        |""".stripMargin)
  }

  test("Simple non-case class") {
    val result = RType.of[FoomNC]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.FoomNC:
        |   fields ->
        |      a: Int
        |      b: String
        |      c (set-only): Option of co.blocke.scala_reflection.models.FoomNC (recursive self-reference)
        |         annotations -> Map(co.blocke.reflect.FieldAnno -> Map(idx -> 0))
        |   non-constructor fields (non-case class) ->
        |      age: Int
        |         annotations -> Map(co.blocke.reflect.FieldAnno -> Map(idx -> 2))
        |      blah: Boolean
        |         annotations -> Map(co.blocke.reflect.DBKey -> Map(), co.blocke.reflect.FieldAnno -> Map(idx -> 5))
        |""".stripMargin)
  }

  test("capture field and class annotations") {
    val result = RType.of[WithAnnotation] 
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.WithAnnotation:
        |   fields ->
        |      id: String
        |         annotations -> Map(co.blocke.reflect.FieldAnno -> Map(idx -> 5))
        |   annotations ->
        |      Map(co.blocke.reflect.ClassAnno -> Map(name -> Foom))
        |""".stripMargin)
  }

  test("sealed abstract class with case class") {
    val result = RType.of[PetOwner]
    assertEquals(result.pretty,
      """co.blocke.scala_reflection.models.PetOwner:
        |   fields ->
        |      owner: String
        |      pet: co.blocke.scala_reflection.models.Animal (sealed abstract class):
        |         fields ->
        |            animalType: String
        |         children ->
        |            co.blocke.scala_reflection.models.Dog:
        |               fields ->
        |                  name: String
        |            co.blocke.scala_reflection.models.Cat:
        |               fields ->
        |                  name: String
        |""".stripMargin)
  }

  test("match / dependent types") {
    val result = RType.of[Definitely]
    assertEquals(result.pretty, """co.blocke.scala_reflection.models.Definitely:
        |   fields ->
        |      id: Int
        |      stuff: Char
        |""".stripMargin)
  }

  test("Ensure caching (equals) works") {
    val r0 = RType.of[Employee]
    val r1 = RType.of[Employee]
    assert(r0 == r1)
    assert(r0.equals(r1))
  }
