package co.blocke.scala_reflection

import munit.*
import models.*

class Inheritance extends munit.FunSuite:

  test("Inheritance and Annotations") {
    val result = RType.of[InheritSimpleChild]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.InheritSimpleChild:
      |   fields ->
      |      extra: String
      |      one: String
      |         annotations -> Map(co.blocke.reflect.Change -> Map(name -> uno), co.blocke.reflect.DBKey -> Map(index -> 50))
      |   non-constructor fields (non-case class) ->
      |      dontForget: Int
      |      foo: Int
      |         annotations -> Map(co.blocke.reflect.DBKey -> Map(index -> 99))
      |      four: Double
      |         annotations -> Map(co.blocke.reflect.DBKey -> Map(index -> 2), co.blocke.reflect.Change -> Map(name -> quatro))
      |      three: Boolean
      |      two: Int
      |         annotations -> Map(co.blocke.reflect.Change -> Map(name -> foobar), co.blocke.reflect.DBKey -> Map(index -> 1))
      |""".stripMargin
    )
  }

  test("Inheritance and Parameterized Classes") {
    val result = RType.of[ParamChild[Boolean]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ParamChild[Boolean]:
      |   fields ->
      |      thing: [T] Boolean
      |   non-constructor fields (non-case class) ->
      |      cosa: Boolean
      |      item: Boolean
      |""".stripMargin
    )
  }
