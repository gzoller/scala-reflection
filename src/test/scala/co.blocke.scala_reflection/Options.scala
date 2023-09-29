package co.blocke.scala_reflection

import munit.*
import rtypes.*
import rtypes.PrimitiveRTypes.*
import models.*
// import java.util.Optional

class Options extends munit.FunSuite:

  test("Simple Option RType") {
    val result = RType.of[Option[String]]
    assertEquals( result.pretty, "Option of String")
  }

  /*
  test("java ArrayList of Option[String]") {
    val result = RType.of[java.util.ArrayList[Option[String]]]
    assertEquals( result.show(), "JavaListInfo(java.util.ArrayList): Option of java.lang.String\n")
  }
  */

  test("Scala optional field") {
    val result = RType.of[NormalOption]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.NormalOption:
      |   fields ->
      |      a: Option of Int
      |      b: String
      |""".stripMargin)
  }

  /*
  test("Java optional field") {
    val result = RType.of[co.blocke.reflect.JavaOption1]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption1):
    |   fields:
    |      (0) fld: Optional of java.lang.Integer
    |""".stripMargin)
  }
  */

  test("Scala nested optional field") {
    val result = RType.of[NestedOption]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.NestedOption:
      |   fields ->
      |      a: Option of Option of Int
      |      b: String
      |""".stripMargin)
  }

  test("Scala option of a class with defaults") {
    val result = RType.of[OptionOfClass]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.OptionOfClass:
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
      |""".stripMargin)
  }

  /*
  test("Java nested optional field") {
    val result = RType.of[co.blocke.reflect.JavaOption2]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption2):
    |   fields:
    |      (0) fld: Optional of Optional of java.lang.Integer
    |""".stripMargin)
  }
  */

  test("Scala optional parameterized field") {
    val result = RType.of[ParamOption[Char]]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.ParamOption[Char]:
      |   fields ->
      |      a: Option of Char
      |""".stripMargin)
  }

  /*
  test("Java optional parameterized field") {
    val result = RType.of[co.blocke.reflect.JavaOption3[Char]]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption3):
    |   fields:
    |      (0) fld: Optional of scala.Char
    |""".stripMargin)
  }

  test("Option assignments in union type") {
    val r = RType.of[UnionHavingOption]
    assert(
      r.constructWith[UnionHavingOption](List(None,Optional.empty())) == UnionHavingOption(None,Optional.empty())
    )
    assert(
      r.constructWith[UnionHavingOption](List(Some(3),Optional.of(3))) == UnionHavingOption(Some(3),Optional.of(3))
    )
  }
  */

  test("Option of a union") {    
    val result = RType.of[OptionHavingUnion]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.OptionHavingUnion:
      |   fields ->
      |      a: Option of Union of:
      |         left--Boolean
      |         right--String
      |""".stripMargin)
  }

  /*
  test("Option of a union assignment") {    
    val r = RType.of[OptionHavingUnion]
    assert(
      r.constructWith[OptionHavingUnion](List(None)) == OptionHavingUnion(None)
    )
    assert(
      r.constructWith[OptionHavingUnion](List(Some(true))) == OptionHavingUnion(Some(true))
    )
    assert(
      r.constructWith[OptionHavingUnion](List(Some("wow"))) == OptionHavingUnion(Some("wow"))
    )
  }
  */