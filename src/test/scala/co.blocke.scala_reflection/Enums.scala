package co.blocke.scala_reflection

import models.*
import rtypes.*

class Enums extends munit.FunSuite:

  test("Java Enums") {
    val result = RType.of[co.blocke.reflect.JavaEnum]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaEnum (Java):
      |   fields ->
      |      color: Enum (Java) having values (RED,GREEN,BLUE)
      |""".stripMargin
    )
  }

  test("Scala Enums (old and new)") {
    val result = RType.of[Birthday]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Birthday:
      |   fields ->
      |      m: Enum (Scala 3) having values (Jan,Feb,Mar)
      |      d: Enumeration (Scala 2) having values (Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday)
      |""".stripMargin
    )
  }

  test("Scala Enum methods") {
    val result = RType.of[Birthday]
    result match {
      case sc: ScalaClassRType[?] =>
        val e = sc.fields(0).fieldType.asInstanceOf[ScalaEnumRType[?]]
        assertEquals(e.ordinal("Feb"), Some(1))
        assertEquals(e.valueAt(2), Some("Mar"))
      case _ => false
    }
  }

  test("Scala Enum ADT") {
    val result = RType.of[ColorSet]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ColorSet:
      |   fields ->
      |      set: Set of Enum (Scala 3) having values (Red,Green,Blue,Mix)
      |""".stripMargin
    )
  }

  test("Scala2 Enumeration methods") {
    val result = RType.of[Birthday]
    result match {
      case sc: ScalaClassRType[?] =>
        val e = sc.fields(1).fieldType.asInstanceOf[ScalaEnumerationRType[?]]
        assertEquals(e.ordinal("Wednesday"), Some(99))
        assertEquals(e.valueAt(99), Some("Wednesday"))
      case _ => false
    }
  }
