package co.blocke.scala_reflection

import munit.*
import rtypes.*
import rtypes.PrimitiveRTypes.*
import models.*
import scala.util.{Left,Right}

class Eithers extends munit.FunSuite:

  test("Scala simple Either field") {
    val result = RType.of[BothSides]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.BothSides:
      |   fields ->
      |      a: Either:
      |         left--Int
      |         right--String
      |""".stripMargin)
  }

  test("Scala Either having a self-reference") {
    val result = RType.of[EitherWithSelf]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.EitherWithSelf:
      |   fields ->
      |      a: co.blocke.scala_reflection.models.Person:
      |         fields ->
      |            name: String
      |            age: Int
      |            item: co.blocke.scala_reflection.models.Item:
      |               fields ->
      |                  desc: String
      |            allDone: Boolean
      |      b: co.blocke.scala_reflection.models.Item (seen before, details above)
      |      c: Either:
      |         left--co.blocke.scala_reflection.models.Person (seen before, details above)
      |         right--co.blocke.scala_reflection.models.Item (seen before, details above)
      |""".stripMargin)
  }

  /*
  test("Scala simple Either field assignment") {
    val r = RType.of[BothSides].asInstanceOf[ScalaCaseClassInfo]
    assert(
      r.constructWith[BothSides](List(Right("Foom"))) == BothSides(Right("Foom"))
    )
    assert(
      r.constructWith[BothSides](List(Left(3))) == BothSides(Left(3))
    )
  }
  */

  test("Scala Either with Option") {
    val result = RType.of[BothSidesWithOption]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.BothSidesWithOption:
      |   fields ->
      |      a: Either:
      |         left--Int
      |         right--Option of String
      |""".stripMargin)
  }

  /*
  test("Scala Either with Option assignment") {
    val r = RType.of[BothSidesWithOption].asInstanceOf[ScalaCaseClassInfo]
    assert(
      r.constructWith[BothSidesWithOption](List(Right(None))) == BothSidesWithOption(Right(None))
    )
    assert(
      r.constructWith[BothSidesWithOption](List(Right(Some("x")))) == BothSidesWithOption(Right(Some("x")))
    )
  }

  test("Scala Either with Union type") {
    val result = RType.of[BothSidesWithUnion]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.BothSidesWithUnion):
    |   fields:
    |      (0) a: Either:
    |         left--scala.Int
    |         right--Union:
    |            left--java.lang.String
    |            right--scala.Boolean
    |""".stripMargin)
  }

  test("Scala Either with Union type assignment") {
    val r = RType.of[BothSidesWithUnion].asInstanceOf[ScalaCaseClassInfo]
    assert(
      r.constructWith[BothSidesWithUnion](List(Right("foo"))) == BothSidesWithUnion(Right("foo"))
    )
    assert(
      r.constructWith[BothSidesWithUnion](List(Right(true))) == BothSidesWithUnion(Right(true))
    )
  }
  */

  test("Scala Either having a parameterized type") {
    val result = RType.of[BothSidesParam[String,Double]]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.BothSidesParam[Y,Z]:
      |   fields ->
      |      a: Either:
      |         left--String
      |         right--Option of co.blocke.scala_reflection.models.ParamOption[T]:
      |               fields ->
      |                  a: Option of Double
      |""".stripMargin)
  }