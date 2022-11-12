package co.blocke.scala_reflection

class PackageObject extends munit.FunSuite:

  test("reflect basic Generic class declared inside Package Object") {
    val result = RType.of[PackageGenericTestClass[Long]]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.package$PackageGenericTestClass):
                                   |   fields:
                                   |      (0)[T] t: scala.Long
                                   |""".stripMargin)
    assertEquals(result.infoClass.getName, "co.blocke.scala_reflection.package$PackageGenericTestClass")
  }
