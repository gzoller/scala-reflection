package co.blocke.scala_reflection

case class Bogus(name: String, ok: Map[String, Int], count: Int)

enum Permissions {
  case READ, WRITE, EXEC
}

@ClassAnno(name = "Foom")
class Person[P](name: String, perm: Permissions, @FieldAnno(idx = 5) age: Array[Int], thing: P):
  type X = String
  var foom: String = "blah"

  // TODO:
  // 1. Java enum (+test)
