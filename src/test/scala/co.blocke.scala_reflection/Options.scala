package co.blocke.scala_reflection

import munit.*
import info.*
import impl.PrimitiveType.*
import java.util.Optional

class Options extends munit.FunSuite:

  test("Option[String]") {
    val result = RType.of[Option[String]]
    assertEquals( result.show(), "Option of java.lang.String\n")
  }

  test("java ArrayList of Option[String]") {
    val result = RType.of[java.util.ArrayList[Option[String]]]
    assertEquals( result.show(), "JavaListInfo(java.util.ArrayList): Option of java.lang.String\n")
  }

  test("Scala optional field") {
    val result = RType.of[NormalOption]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.NormalOption):
    |   fields:
    |      (0) a: Option of scala.Int
    |""".stripMargin)
  }

  test("Java optional field") {
    val result = RType.of[co.blocke.reflect.JavaOption1]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption1):
    |   fields:
    |      (0) fld: Optional of java.lang.Integer
    |""".stripMargin)
  }

  test("Scala nested optional field") {
    val result = RType.of[NestedOption]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.NestedOption):
    |   fields:
    |      (0) a: Option of Option of scala.Int
    |""".stripMargin)
  }

  test("Java nested optional field") {
    val result = RType.of[co.blocke.reflect.JavaOption2]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption2):
    |   fields:
    |      (0) fld: Optional of Optional of java.lang.Integer
    |""".stripMargin)
  }

  test("Scala optional parameterized field") {
    val result = RType.of[ParamOption[Char]]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.ParamOption):
    |   fields:
    |      (0) a: Option of scala.Char
    |""".stripMargin)
  }

  test("Java optional parameterized field") {
    val result = RType.of[co.blocke.reflect.JavaOption3[Char]]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaOption3):
    |   fields:
    |      (0) fld: Optional of scala.Char
    |""".stripMargin)
  }

  test("Option assignments in union type") {
    val r = RType.of[UnionHavingOption].asInstanceOf[ScalaCaseClassInfo]
    assert(
      r.constructWith[UnionHavingOption](List(None,Optional.empty())) == UnionHavingOption(None,Optional.empty())
    )
    assert(
      r.constructWith[UnionHavingOption](List(Some(3),Optional.of(3))) == UnionHavingOption(Some(3),Optional.of(3))
    )
  }

  test("Option of a union") {    
    val result = RType.of[OptionHavingUnion]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.OptionHavingUnion):
    |   fields:
    |      (0) a: Option of Union:
    |         left--scala.Boolean
    |         right--java.lang.String
    |""".stripMargin)
  }

  test("Option of a union assignment") {    
    val r = RType.of[OptionHavingUnion].asInstanceOf[ScalaCaseClassInfo]
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