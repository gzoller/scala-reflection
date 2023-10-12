package co.blocke.scala_reflection

import munit.*
import rtypes.*
import models.*
// import java.util.Optional

class Options extends munit.FunSuite:

  test("Simple Option RType") {
    val result = RType.of[Option[String]]
    assertEquals(result.pretty, "Option of String")
  }

  test("java ArrayList of Option[String]") {
    val result = RType.of[java.util.ArrayList[Option[String]]]
    assertEquals(result.pretty, "Java ArrayList of: Option of String")
  }

  test("Scala optional field") {
    val result = RType.of[NormalOption]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.NormalOption:
      |   fields ->
      |      a: Option of Int
      |      b: String
      |""".stripMargin
    )
  }

  test("Java optional field") {
    val result = RType.of[co.blocke.reflect.JavaOption1]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaOption1 (Java):
      |   fields ->
      |      fld: Optional of Integer
      |""".stripMargin
    )
  }

  test("Scala nested optional field") {
    val result = RType.of[NestedOption]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.NestedOption:
      |   fields ->
      |      a: Option of Option of Int
      |      b: String
      |""".stripMargin
    )
  }

  test("Scala option of a class with defaults") {
    val result = RType.of[OptionOfClass]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.OptionOfClass:
      |   fields ->
      |      a: Option of co.blocke.scala_reflection.models.Person:
      |         fields ->
      |            name: String
      |            age: Int
      |            item: co.blocke.scala_reflection.models.Item:
      |               fields ->
      |                  desc: String
      |            allDone: Boolean
      |         (default value: Some(Person(Mike,34,...)
      |      b: Option of co.blocke.scala_reflection.models.Person (seen before, details above)
      |         (default value: None)
      |      c: String
      |""".stripMargin
    )
  }

  test("Java nested optional field") {
    val result = RType.of[co.blocke.reflect.JavaOption2]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaOption2 (Java):
      |   fields ->
      |      fld: Optional of Optional of Integer
      |""".stripMargin
    )
  }

  test("Scala optional parameterized field") {
    val result = RType.of[ParamOption[Char]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ParamOption[Char]:
      |   fields ->
      |      a: Option of Char
      |""".stripMargin
    )
  }

  test("Java optional parameterized field") {
    val result = RType.of[co.blocke.reflect.JavaOption3[Char]]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaOption3[Char] (Java):
      |   fields ->
      |      fld: Optional of Char
      |""".stripMargin
    )
  }

  test("Option of a union") {
    val result = RType.of[OptionHavingUnion]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.OptionHavingUnion:
      |   fields ->
      |      a: Option of Union of:
      |         left--Boolean
      |         right--String
      |""".stripMargin
    )
  }
