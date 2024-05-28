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
    val ord = result.asInstanceOf[JavaClassRType[_]].fields(0).asInstanceOf[NonConstructorFieldInfo].fieldType.asInstanceOf[JavaEnumRType[_]].ordinal("GREEN").get
    assertEquals(ord, 1)
    assertEquals(
      result.asInstanceOf[JavaClassRType[_]].fields(0).asInstanceOf[NonConstructorFieldInfo].fieldType.asInstanceOf[JavaEnumRType[_]].valueAt(2).map(_.toString),
      Some("BLUE")
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

  test("Enum Json generation") {
    val js = RType.ofJS[Birthday]
    assertEquals(
      js,
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Birthday","typedName":"co.blocke.scala_reflection.models.Birthday","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"m","fieldType":{"rtype":"ScalaEnumRef","name":"co.blocke.scala_reflection.models.Month","typedName":"co.blocke.scala_reflection.models.Month","values":["Jan","Feb","Mar"]},"originalSymbol":null,"annotations":{}},{"name":"d","fieldType":{"rtype":"ScalaEnumerationRef","name":"co.blocke.scala_reflection.models.WeekDay","typedName":"co.blocke.scala_reflection.models.WeekDay","values":["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
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
