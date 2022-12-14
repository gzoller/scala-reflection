package co.blocke.scala_reflection

import munit.*
import info.*
import impl.PrimitiveType.*

class Enums extends munit.FunSuite:

  test("Java Enums") {
    val result = RType.of[co.blocke.reflect.JavaEnum]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaEnum):
    |   fields:
    |      (0) color: JavaEnumInfo(co.blocke.reflect.Color)
    |""".stripMargin)
  }

  test("Scala Enums (old and new)") {
    val result = RType.of[Birthday]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Birthday):
    |   fields:
    |      (0) m: ScalaEnumInfo(co.blocke.scala_reflection.Month) with values [Jan,Feb,Mar]
    |      (1) d: ScalaEnumerationInfo(co.blocke.scala_reflection.WeekDay) with values [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
    |""".stripMargin)
  }

  test("Scala Enum methods") {
    val result = RType.of[Birthday]
    result match {
      case sc: ScalaCaseClassInfo =>
        val e = sc.fields(0).asInstanceOf[ScalaFieldInfo].fieldType.asInstanceOf[ScalaEnumInfo]
        assertEquals(e.valueOf("Jan"), Month.Jan)
        assertEquals(e.ordinal("Feb"), 1)
        assertEquals(e.valueOf(2), Month.Mar)
      case _ => false
    }
  }

  test("Scala Enum ADT") {
    val result = RType.of[ColorSet]
    assertEquals(result.show(),
      """ScalaCaseClassInfo(co.blocke.scala_reflection.ColorSet):
        |   fields:
        |      (0) set: SeqLikeInfo(scala.collection.immutable.Set): ScalaEnumInfo(co.blocke.scala_reflection.Color) with values [Red,Green,Blue,Mix]
        |""".stripMargin)
  }

  test("Scala2 Enumeration methods") {
    val result = RType.of[Birthday]
    result match {
      case sc: ScalaCaseClassInfo => 
        val e = sc.fields(1).asInstanceOf[ScalaFieldInfo].fieldType.asInstanceOf[ScalaEnumerationInfo]
        assertEquals( e.valueOf("Monday"), WeekDay.Monday )
        assertEquals( e.ordinal("Wednesday"), 99 )
        assertEquals( e.valueOf(99), WeekDay.Wednesday )
      case _ => false
    }
  }