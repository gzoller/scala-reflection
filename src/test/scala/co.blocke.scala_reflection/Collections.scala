package co.blocke.scala_reflection

import munit.*
import info.*
import impl.PrimitiveType.*

class Collections extends munit.FunSuite:

  test("Scala List") {
    val result = RType.of[Coll1]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll1):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.immutable.List): java.lang.String
    |""".stripMargin)
  }

  test("Scala Set") {
    val result = RType.of[Coll2]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll2):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.immutable.HashSet): java.lang.String
    |""".stripMargin)
  }

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

  test("Scala mutable List") {
    val result = RType.of[Coll1m]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll1m):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.mutable.ListBuffer): java.lang.String
    |""".stripMargin)
  }

  test("Scala mutable Set") {
    val result = RType.of[Coll2m]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Coll2m):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.mutable.HashSet): java.lang.String
    |""".stripMargin)
  }

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
    |      (0) a: MapLikeInfo(scala.collection.mutable.ListMap):
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

  test("Tuples") {
    val result = RType.of[TupleTurtle[Boolean]]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.TupleTurtle):
    |   fields:
    |      (0) t: (
    |         scala.Int
    |         scala.Boolean
    |         SeqLikeInfo(scala.collection.immutable.List): java.lang.String
    |         ScalaCaseClassInfo(co.blocke.scala_reflection.NormalOption):
    |            fields:
    |               (0) a: Option of scala.Int
    |         )
    |""".stripMargin)
  }

  test("Scala Arrays") {
    val result = RType.of[WithScalaArray]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.WithScalaArray):
    |   fields:
    |      (0) list: array of array of scala.Char
    |      (1) x1: array of scala.Boolean
    |      (2) x2: array of scala.Byte
    |      (3) x3: array of scala.Char
    |      (4) x4: array of scala.Double
    |      (5) x5: array of scala.Float
    |      (6) x6: array of scala.Int
    |      (7) x7: array of scala.Long
    |      (8) x8: array of scala.Short
    |      (9) x9: array of java.lang.String
    |""".stripMargin)
  }