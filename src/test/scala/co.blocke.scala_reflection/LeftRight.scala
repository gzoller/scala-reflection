package co.blocke.scala_reflection

import munit.*
import rtypes.*
import models.*
import scala.util.{Left, Right}

class LeftRight extends munit.FunSuite:

  test("Scala simple Either field") {
    val result = RType.of[BothSides]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.BothSides:
      |   fields ->
      |      a: Either of:
      |         left--Int
      |         right--String
      |""".stripMargin
    )
  }

  test("Scala Either having a self-reference") {
    val result = RType.of[EitherWithSelf]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.EitherWithSelf:
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
      |         unique field hash count-- 2
      |         left--co.blocke.scala_reflection.models.Person (seen before, details above)
      |         right--co.blocke.scala_reflection.models.Item (seen before, details above)
      |""".stripMargin
    )
  }

  test("Scala Either with Option") {
    val result = RType.of[BothSidesWithOption]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.BothSidesWithOption:
      |   fields ->
      |      a: Either of:
      |         left--Int
      |         right--Option of String
      |""".stripMargin
    )
  }

  test("Scala Either with Union type") {
    val result = RType.of[BothSidesWithUnion]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.BothSidesWithUnion:
      |   fields ->
      |      a: Either of:
      |         left--Int
      |         right--Union of:
      |               left--String
      |               right--Boolean
      |""".stripMargin
    )
  }

  test("Scala Either having a parameterized type") {
    val result = RType.of[BothSidesParam[String, Double]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.BothSidesParam[String,Double]:
      |   fields ->
      |      a: Either of:
      |         unique field hash count-- 1
      |         left--String
      |         right--Option of co.blocke.scala_reflection.models.ParamOption[Double]:
      |               fields ->
      |                  a: Option of Double
      |""".stripMargin
    )
  }

  test("Scala Union type") {
    val result = RType.of[Together]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Together:
      |   fields ->
      |      a: Intersection of:
      |         unique field hash count-- 1
      |         left--Int
      |         right--co.blocke.scala_reflection.models.Person:
      |               fields ->
      |                  name: String
      |                  age: Int
      |                  item: co.blocke.scala_reflection.models.Item:
      |                     fields ->
      |                        desc: String
      |                  allDone: Boolean
      |""".stripMargin
    )
  }

  test("Scala Intersection type with Option") {
    val result = RType.of[Apart]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Apart:
      |   fields ->
      |      a: Union of:
      |         unique field hash count-- 1
      |         left--Option of co.blocke.scala_reflection.models.Person:
      |               fields ->
      |                  name: String
      |                  age: Int
      |                  item: co.blocke.scala_reflection.models.Item:
      |                     fields ->
      |                        desc: String
      |                  allDone: Boolean
      |         right--String
      |""".stripMargin
    )
  }

  test("Scala Intersection type with type parameters") {
    val result = RType.of[ApartWithType[Int, Boolean]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ApartWithType[Int,Boolean]:
      |   fields ->
      |      a: Union of:
      |         unique field hash count-- 1
      |         left--Option of co.blocke.scala_reflection.models.Thingy[Int]:
      |               fields ->
      |                  name: String
      |                  payload: [Z] Int
      |         right--Boolean
      |""".stripMargin
    )
  }
  test("Scala Intersection type with type parameters (2 classes in LR)") {
    val result = RType.of[ApartWithType[Int, Dog]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ApartWithType[Int,Dog]:
        |   fields ->
        |      a: Union of:
        |         unique field hash count-- 2
        |         left--Option of co.blocke.scala_reflection.models.Thingy[Int]:
        |               fields ->
        |                  name: String
        |                  payload: [Z] Int
        |         right--co.blocke.scala_reflection.models.Dog:
        |               fields ->
        |                  name: String
        |""".stripMargin
    )
  }

  test("opaque type alias is a union type") {
    val result = RType.of[OpaqueUnion]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.OpaqueUnion:
      |   fields ->
      |      id: alias GEN_ID defined as Union of:
      |         left--Int
      |         right--String
      |""".stripMargin
    )
  }
