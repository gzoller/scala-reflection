package co.blocke.scala_reflection

import munit.*
import models.*
import co.blocke.reflect.* 
import co.blocke.scala_reflection.rtypes.JavaClassRType

class Java extends munit.FunSuite:

  val c1 = Class.forName("co.blocke.reflect.Hey")

  test("basic Java collections") {
    val result = RType.of[JColl]
    assertEquals( result.pretty, """co.blocke.scala_reflection.models.JColl:
      |   fields ->
      |      a: Java List of: Int
      |      b: Optional of Java ArrayList of: Int
      |      c: Java Stack of: String
      |      d: Java Queue of: Map of:
      |         key: Int
      |         value: String
      |      e: Java Set of: Boolean
      |      f: Java Map of:
      |         key: Int
      |         value: String
      |""".stripMargin)
  }

  test("Simple parameter substitution") {
    val result = RType.of[JavaParam[String]]
    assertEquals( result.pretty, """co.blocke.reflect.JavaParam[String] (Java):
      |   fields ->
      |      jThing: String
      |""".stripMargin)
  }

  test("Nested Java classes") {
    val result = RType.of[co.blocke.reflect.You]
    assertEquals( result.pretty, """co.blocke.reflect.You (Java):
      |   fields ->
      |      sayHey: co.blocke.reflect.Hey (Java):
      |         fields ->
      |            jString: String
      |""".stripMargin)
  }

  test("Java collection types") {
    val result = RType.of[JavaCollections]
    assertEquals( result.pretty, """co.blocke.reflect.JavaCollections (Java):
      |   fields ->
      |      hMap: Java HashMap of:
      |         key: String
      |         value: Integer
      |      myArr: Array of: String
      |      myList: Java ArrayList of: String
      |      myQ: Java concurrent BlockingQueue of: String
      |      myTree: Java TreeSet of: String
      |      nested: Array of: Java List of: Integer
      |      pushPop: Java Stack of: Long
      |""".stripMargin)
  }

  test("Detect parameterized Java class") {
    val result = RType.of("co.blocke.reflect.ParamAnno") 
    assertEquals( result.pretty, """co.blocke.reflect.ParamAnno[T] (Java):
      |   fields ->
      |      age: T
      |         annotations -> Map(co.blocke.reflect.FieldAnno -> Map(idx -> 2))
      |      name: String
      |         annotations -> Map(co.blocke.reflect.FieldAnno -> Map(idx -> 1))
      |   annotations ->
      |      Map(co.blocke.reflect.ClassAnno -> Map(name -> Foom))
      |""".stripMargin)
  }

  test("reflect basic with capture") {
    val result = RType.of[co.blocke.reflect.Person]
    println(result)
    assertEquals( result.pretty, """co.blocke.reflect.Person (Java):
      |   fields ->
      |      age: Int
      |      name: String
      |      other: Int
      |""".stripMargin)
    assert(result.asInstanceOf[JavaClassRType[_]].mixins.contains("co.blocke.scala_reflection.models.SJCaptureJava"))
  }

  test("Java parameterized class field member") {
    val result = RType.of[co.blocke.reflect.JavaParamHolder]
    assertEquals( result.pretty, """co.blocke.reflect.JavaParamHolder (Java):
      |   fields ->
      |      jFoo: co.blocke.reflect.JavaParam[Integer] (Java):
      |         fields ->
      |            jThing: Integer
      |""".stripMargin)
  }

  test("Java parameterized class field member (Wildcard)") {
    println("HERE: "+classOf[JavaParam[Boolean]])
    println(Class.forName("co.blocke.reflect.JavaParam"))
    val result = RType.of[co.blocke.reflect.JavaParamHolder2]
    assertEquals( result.pretty, """co.blocke.reflect.JavaParamHolder2 (Java):
      |   fields ->
      |      jFoo: co.blocke.reflect.JavaParam[Object] (Java):
      |         fields ->
      |            jThing: K
      |""".stripMargin)
  }