package co.blocke.scala_reflection

import munit.*
import rtypes.*
import rtypes.PrimitiveRTypes.*
import models.*

class Collections extends munit.FunSuite:

  test("Scala List") {
    val result = RType.of[Coll1]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.Coll1:
      |   fields ->
      |      a: List of: String
      |""".stripMargin)
  }

  test("Scala Set") {
    val result = RType.of[Coll2]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.Coll2:
      |   fields ->
      |      a: HashSet of: String
      |""".stripMargin)
  }

  /*
  test("Scala Map 1") {
    val result = RType.of[Coll3]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll3):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.immutable.Map):
    |         java.lang.String
    |         scala.Float
    |""".stripMargin)  
  }

  test("Scala Map 2") {
    val result = RType.of[Coll4]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll4):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.immutable.ListMap):
    |         java.lang.String
    |         scala.Boolean
    |""".stripMargin)  
  }
  */

  test("Scala mutable List") {
    val result = RType.of[Coll1m]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.Coll1m:
      |   fields ->
      |      a: mutable ListBuffer of: String
      |""".stripMargin)
  }

  test("Scala mutable Set") {
    val result = RType.of[Coll2m]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.Coll2m:
      |   fields ->
      |      a: mutable HashSet of: String
      |""".stripMargin)
  }

  /*
  test("Scala mutable Map 1") {
    val result = RType.of[Coll3m]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll3m):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.mutable.Map):
    |         java.lang.String
    |         scala.Float
    |""".stripMargin)
  }

  test("Scala mutable Map 2") {
    val result = RType.of[Coll4m]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll4m):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.mutable.HashMap):
    |         java.lang.String
    |         scala.Boolean
    |""".stripMargin)
  }

  test("Nested Collections") {
    val result = RType.of[NestedColl]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.NestedColl):
    |   fields:
    |      (0) a: MapLikeInfo(scala.collection.immutable.Map):
    |         java.lang.String
    |         SeqLikeInfo(scala.collection.immutable.List): Option of scala.Int
    |""".stripMargin)
  }
  */

  test("Tuples") {
    val result = RType.of[TupleTurtle[Boolean]]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.TupleTurtle[Z]:
      |   fields ->
      |      t: Tuple of:
      |         0: Int
      |         1: Boolean
      |         2: List of: String
      |         3: co.blocke.scala_reflection.models.NormalOption:
      |            fields ->
      |               a: Option of Int
      |               b: String
      |""".stripMargin)
  }

  test("Scala Arrays") {
    val result = RType.of[WithScalaArray]
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.WithScalaArray:
      |   fields ->
      |      list: Array of: Array of: Char
      |      x1: Array of: Boolean
      |      x2: Array of: Byte
      |      x3: Array of: Char
      |      x4: Array of: Double
      |      x5: Array of: Float
      |      x6: Array of: Int
      |      x7: Array of: Long
      |      x8: Array of: Short
      |      x9: Array of: String
      |""".stripMargin)
  }
