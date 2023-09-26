package co.blocke.scala_reflection

import munit.*
import rtypes.*
import rtypes.PrimitiveRTypes.*
import models.*
import scala.util.{Left,Right}

class LeftRight extends munit.FunSuite:

  test("Scala simple Either field") {
    val result = RType.of[BothSides]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.BothSides:
      |   fields ->
      |      a: Either of:
      |         left--Int
      |         right--String
      |""".stripMargin)
  }

  test("Scala Either having a self-reference") {
    val result = RType.of[EitherWithSelf]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.EitherWithSelf:
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
      |      c: Either of:
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
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.BothSidesWithOption:
      |   fields ->
      |      a: Either of:
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
  */

  test("Scala Either with Union type") {
    val result = RType.of[BothSidesWithUnion]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.BothSidesWithUnion:
      |   fields ->
      |      a: Either of:
      |         left--Int
      |         right--Union of:
      |               left--String
      |               right--Boolean
      |""".stripMargin)
  }

  /*
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
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.BothSidesParam[String,Double]:
      |   fields ->
      |      a: Either of:
      |         left--String
      |         right--Option of co.blocke.scala_reflection.models.ParamOption[Double]:
      |               fields ->
      |                  a: Option of Double
      |""".stripMargin)
  }

  test("Scala Union type") {
    val result = RType.of[Together]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.Together:
      |   fields ->
      |      a: Intersection of:
      |         left--Int
      |         right--co.blocke.scala_reflection.models.Person:
      |               fields ->
      |                  name: String
      |                  age: Int
      |                  item: co.blocke.scala_reflection.models.Item:
      |                     fields ->
      |                        desc: String
      |                  allDone: Boolean
      |""".stripMargin)
  }

  test("Scala Intersection type with Option") {
    val result = RType.of[Apart]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.Apart:
      |   fields ->
      |      a: Union of:
      |         left--Option of co.blocke.scala_reflection.models.Person:
      |               fields ->
      |                  name: String
      |                  age: Int
      |                  item: co.blocke.scala_reflection.models.Item:
      |                     fields ->
      |                        desc: String
      |                  allDone: Boolean
      |         right--String
      |""".stripMargin)
  }

  test("Scala Intersection type with type parameters") {
    val result = RType.of[ApartWithType[Int,Boolean]]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.ApartWithType[Int,Boolean]:
      |   fields ->
      |      a: Union of:
      |         left--Option of co.blocke.scala_reflection.models.Thingy[Int]:
      |               fields ->
      |                  name: String
      |                  payload: [Z] Int
      |         right--Boolean
      |""".stripMargin)
  }

  test("opaque type alias is a union type") {
    val result = RType.of[OpaqueUnion] 
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.OpaqueUnion:
      |   fields ->
      |      id: alias GEN_ID defined as Union of:
      |         left--Int
      |         right--String
      |""".stripMargin)
  }
