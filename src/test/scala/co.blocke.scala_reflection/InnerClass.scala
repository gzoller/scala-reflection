package co.blocke.scala_reflection

object InnerClass:

  trait Trait:
    abstract class Field protected(name: String)

  object MyObject extends Trait:
    val field = new Field("John Doe") {}

class InnerClass extends munit.FunSuite:

  test("Scala Inner Types") {
    val result = RType.of[InnerClass.MyObject.field.type]
    assert(result.infoClass ne null)
  }
