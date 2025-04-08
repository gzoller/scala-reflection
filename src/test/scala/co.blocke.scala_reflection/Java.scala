package co.blocke.scala_reflection

import munit.*
import models.*
import co.blocke.reflect.*
import co.blocke.scala_reflection.rtypes.JavaClassRType

class Java extends munit.FunSuite:

  val c1 = Class.forName("co.blocke.reflect.Hey")

  test("basic Java collections") {
    val result = RType.of[JColl]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.JColl:
      |   fields ->
      |      a: Java List of Int
      |      b: Optional of Java ArrayList of Int
      |      c: Java Stack of String
      |      d: Java Queue of Map of: (preserve order: false)
      |         key: Int
      |         value: String
      |      e: Java Set of Boolean
      |      f: Java Map of: (preserve order: false)
      |         key: Int
      |         value: String
      |      g: Java LinkedHashMap of: (preserve order: true)
      |         key: String
      |         value: Int
      |""".stripMargin
    )
  }

  test("Simple parameter substitution") {
    val result = RType.of[JavaParam[String]]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaParam[String] (Java):
      |   fields ->
      |      jThing: String
      |""".stripMargin
    )
  }

  test("Nested Java classes") {
    val result = RType.of[co.blocke.reflect.You]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.You (Java):
      |   fields ->
      |      sayHey: co.blocke.reflect.Hey (Java):
      |         fields ->
      |            jString: String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[co.blocke.reflect.You],
      """{"rtype":"JavaClassRType","name":"co.blocke.reflect.You","typedName":"co.blocke.reflect.You","typeParamSymbols":[],"typeParamValues":[],"fields":[{"name":"sayHey","fieldType":{"rtype":"JavaClassRType","name":"co.blocke.reflect.Hey","typedName":"co.blocke.reflect.Hey","typeParamSymbols":[],"typeParamValues":[],"fields":[{"name":"jString","fieldType":{"rtype":"StringRType","name":"java.lang.String"},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object"]},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object"]}"""
    )
  }

  test("Java collection types") {
    val result = RType.of[JavaCollections]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaCollections (Java):
      |   fields ->
      |      hMap: Java HashMap of: (preserve order: false)
      |         key: String
      |         value: Integer (Java)
      |      myArr: Array of String
      |      myList: Java ArrayList of String
      |      myQ: Java concurrent BlockingQueue of String
      |      myTree: Java TreeSet of String
      |      nested: Array of Java List of Integer (Java)
      |      pushPop: Java Stack of Long (Java)
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[JavaCollections],
      """{"rtype":"JavaClassRType","name":"co.blocke.reflect.JavaCollections","typedName":"co.blocke.reflect.JavaCollections","typeParamSymbols":[],"typeParamValues":[],"fields":[{"name":"hMap","fieldType":{"rtype":"JavaMapRType","name":"java.util.HashMap","typedName":"java.util.HashMap[java.lang.String,java.lang.Integer]","isOrdered":false,"typeParamSymbols":["K","V"],"elementType":{"rtype":"StringRType","name":"java.lang.String"},"elementType2":{"rtype":"JavaIntegerRType","name":"java.lang.Integer"}},"originalSymbol":null,"annotations":{}},{"name":"myArr","fieldType":{"rtype":"ArrayRType","name":"[Ljava.lang.String;","typedName":"[Ljava.lang.String;[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}},{"name":"myList","fieldType":{"rtype":"JavaCollectionRType","name":"java.util.ArrayList","typedName":"java.util.ArrayList[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}},{"name":"myQ","fieldType":{"rtype":"JavaCollectionRType","name":"java.util.concurrent.BlockingQueue","typedName":"java.util.concurrent.BlockingQueue[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}},{"name":"myTree","fieldType":{"rtype":"JavaCollectionRType","name":"java.util.TreeSet","typedName":"java.util.TreeSet[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}},{"name":"nested","fieldType":{"rtype":"ArrayRType","name":"[Ljava.util.List;","typedName":"[Ljava.util.List;[java.util.List[java.lang.Integer]]","typeParamSymbols":["A"],"elementType":{"rtype":"JavaCollectionRType","name":"java.util.List","typedName":"java.util.List[java.lang.Integer]","typeParamSymbols":["A"],"elementType":{"rtype":"JavaIntegerRType","name":"java.lang.Integer"}}},"originalSymbol":null,"annotations":{}},{"name":"pushPop","fieldType":{"rtype":"JavaCollectionRType","name":"java.util.Stack","typedName":"java.util.Stack[java.lang.Long]","typeParamSymbols":["A"],"elementType":{"rtype":"JavaLongRType","name":"java.lang.Long"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object"]}"""
    )
  }

  test("Detect parameterized Java class") {
    val result = RType.of(Class.forName("co.blocke.reflect.ParamAnno"))
    assertEquals(
      result.pretty,
      """co.blocke.reflect.ParamAnno[T] (Java):
      |   fields ->
      |      age: T
      |         annotations -> Map(co.blocke.reflect.FieldAnno -> Map(idx -> 2))
      |      name: String
      |         annotations -> Map(co.blocke.reflect.FieldAnno -> Map(idx -> 1))
      |   annotations ->
      |      Map(co.blocke.reflect.ClassAnno -> Map(name -> Foom))
      |""".stripMargin
    )
  }

  test("reflect basic with capture") {
    val result = RType.of[co.blocke.reflect.Person]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.Person (Java):
      |   fields ->
      |      age: Int
      |      name: String
      |      other: Int
      |""".stripMargin
    )
    assert(result.asInstanceOf[JavaClassRType[?]].mixins.contains("co.blocke.scala_reflection.models.SJCaptureJava"))
  }

  test("Java parameterized class field member") {
    val result = RType.of[co.blocke.reflect.JavaParamHolder]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaParamHolder (Java):
      |   fields ->
      |      jFoo: co.blocke.reflect.JavaParam[Integer] (Java):
      |         fields ->
      |            jThing: Integer (Java)
      |""".stripMargin
    )
  }

  test("Java parameterized class field member (Wildcard)") {
    val result = RType.of[co.blocke.reflect.JavaParamHolder2]
    assertEquals(
      result.pretty,
      """co.blocke.reflect.JavaParamHolder2 (Java):
      |   fields ->
      |      jFoo: co.blocke.reflect.JavaParam[Object] (Java):
      |         fields ->
      |            jThing: K
      |""".stripMargin
    )
  }
