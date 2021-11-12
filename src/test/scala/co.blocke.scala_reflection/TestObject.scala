package co.blocke.scala_reflection

object TestObject {
  private case class GenericTestClass[T](t: T)
}

class TestObject extends munit.FunSuite:

  test("reflect basic Generic class declared inside Object") {
    val result = RType.of[TestObject.GenericTestClass[Int]]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.TestObject$.GenericTestClass):
                                   |   fields:
                                   |      (0)[T] t: scala.Int
                                   |""".stripMargin)
    assertEquals(result.infoClass.getName, "co.blocke.scala_reflection.TestObject$GenericTestClass")
  }
