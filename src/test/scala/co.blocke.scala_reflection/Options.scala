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
    assertEquals(result.pretty, "Java ArrayList of Option of String")
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
    assertEquals(
      RType.ofJS[NormalOption],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.NormalOption","typedName":"co.blocke.scala_reflection.models.NormalOption","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"ScalaOptionRType","name":"scala.Option","typedName":"scala.Option[scala.Int]","typeParamSymbols":["A"],"optionParamType":{"rtype":"IntRType","name":"scala.Int"}},"originalSymbol":null,"annotations":{}},{"name":"b","fieldType":{"rtype":"StringRType","name":"java.lang.String"},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Java optional field") {
    val result = RType.of[co.blocke.reflect.JavaOption1]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaOption1 (Java):
      |   fields ->
      |      fld: Optional of Integer (Java)
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[co.blocke.reflect.JavaOption1],
      """{"rtype":"JavaClassRType","name":"co.blocke.reflect.JavaOption1","typedName":"co.blocke.reflect.JavaOption1","typeParamSymbols":[],"typeParamValues":[],"fields":[{"name":"fld","fieldType":{"rtype":"JavaOptionalRType","name":"java.util.Optional","typedName":"java.util.Optional[java.lang.Integer]","typeParamSymbols":["A"],"optionParamType":{"rtype":"JavaIntegerRType","name":"java.lang.Integer"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object"]}"""
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
      |      fld: Optional of Optional of Integer (Java)
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
