package co.blocke.scala_reflection

import munit.*
import rtypes.*
import models.*

class Collections extends munit.FunSuite:

  test("Scala List") {
    val result = RType.of[Coll1]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll1:
      |   fields ->
      |      a: List of String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll1],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll1","typedName":"co.blocke.scala_reflection.models.Coll1","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"SeqRType","name":"scala.collection.immutable.List","typedName":"scala.collection.immutable.List[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala Set") {
    val result = RType.of[Coll2]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll2:
      |   fields ->
      |      a: HashSet of String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll2],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll2","typedName":"co.blocke.scala_reflection.models.Coll2","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"SetRType","name":"scala.collection.immutable.HashSet","typedName":"scala.collection.immutable.HashSet[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala Iterable") {
    val result = RType.of[Coll5]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll5:
      |   fields ->
      |      a: scala.collection.Iterable of String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll5],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll5","typedName":"co.blocke.scala_reflection.models.Coll5","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"IterableRType","name":"scala.collection.Iterable","typedName":"scala.collection.Iterable[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala Map 1") {
    val result = RType.of[Coll3]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll3:
      |   fields ->
      |      a: Map of: (preserve order: false)
      |         key: String
      |         value: Float
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll3],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll3","typedName":"co.blocke.scala_reflection.models.Coll3","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"MapRType","name":"scala.collection.immutable.Map","typedName":"scala.collection.immutable.Map[java.lang.String,scala.Float]","isOrdered":false,"typeParamSymbols":["K","V"],"elementType":{"rtype":"StringRType","name":"java.lang.String"},"elementType2":{"rtype":"FloatRType","name":"scala.Float"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala Map 2") {
    val result = RType.of[Coll4]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll4:
      |   fields ->
      |      a: ListMap of: (preserve order: true)
      |         key: String
      |         value: Boolean
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll4],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll4","typedName":"co.blocke.scala_reflection.models.Coll4","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"MapRType","name":"scala.collection.immutable.ListMap","typedName":"scala.collection.immutable.ListMap[java.lang.String,scala.Boolean]","isOrdered":true,"typeParamSymbols":["K","V"],"elementType":{"rtype":"StringRType","name":"java.lang.String"},"elementType2":{"rtype":"BooleanRType","name":"scala.Boolean"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala mutable List") {
    val result = RType.of[Coll1m]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll1m:
      |   fields ->
      |      a: mutable ListBuffer of String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll1m],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll1m","typedName":"co.blocke.scala_reflection.models.Coll1m","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"SeqRType","name":"scala.collection.mutable.ListBuffer","typedName":"scala.collection.mutable.ListBuffer[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala mutable Set") {
    val result = RType.of[Coll2m]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll2m:
      |   fields ->
      |      a: mutable HashSet of String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll2m],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll2m","typedName":"co.blocke.scala_reflection.models.Coll2m","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"SetRType","name":"scala.collection.mutable.HashSet","typedName":"scala.collection.mutable.HashSet[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala mutable Map 1") {
    val result = RType.of[Coll3m]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll3m:
      |   fields ->
      |      a: mutable Map of: (preserve order: false)
      |         key: String
      |         value: Float
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll3m],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll3m","typedName":"co.blocke.scala_reflection.models.Coll3m","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"MapRType","name":"scala.collection.mutable.Map","typedName":"scala.collection.mutable.Map[java.lang.String,scala.Float]","isOrdered":false,"typeParamSymbols":["K","V"],"elementType":{"rtype":"StringRType","name":"java.lang.String"},"elementType2":{"rtype":"FloatRType","name":"scala.Float"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala mutable Map 2") {
    val result = RType.of[Coll4m]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Coll4m:
      |   fields ->
      |      a: mutable HashMap of: (preserve order: false)
      |         key: String
      |         value: Boolean
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[Coll4m],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.Coll4m","typedName":"co.blocke.scala_reflection.models.Coll4m","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"MapRType","name":"scala.collection.mutable.HashMap","typedName":"scala.collection.mutable.HashMap[java.lang.String,scala.Boolean]","isOrdered":false,"typeParamSymbols":["K","V"],"elementType":{"rtype":"StringRType","name":"java.lang.String"},"elementType2":{"rtype":"BooleanRType","name":"scala.Boolean"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Nested Collections") {
    val result = RType.of[NestedColl]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.NestedColl:
      |   fields ->
      |      a: Map of: (preserve order: false)
      |         key: String
      |         value: List of Option of Int
      |""".stripMargin
    )
  }

  test("Tuples") {
    val result = RType.of[TupleTurtle[Boolean]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.TupleTurtle[Boolean]:
      |   fields ->
      |      t: Tuple of:
      |         0: Int
      |         1: Boolean
      |         2: List of String
      |         3: co.blocke.scala_reflection.models.NormalOption:
      |            fields ->
      |               a: Option of Int
      |               b: String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[TupleTurtle[Boolean]],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.TupleTurtle","typedName":"co.blocke.scala_reflection.models.TupleTurtle[scala.Boolean]","typeParamSymbols":["Z"],"typeParamValues":[{"rtype":"BooleanRType","name":"scala.Boolean"}],"typeMembers":[],"fields":[{"name":"t","fieldType":{"rtype":"TupleRType","name":"scala.Tuple4","typedName":"scala.Tuple4[scala.Int,scala.Boolean,scala.collection.immutable.List[java.lang.String],co.blocke.scala_reflection.models.NormalOption]","typeParamSymbols":["A","B","C","D"],"tupleTypes":[{"rtype":"IntRType","name":"scala.Int"},{"rtype":"BooleanRType","name":"scala.Boolean"},{"rtype":"SeqRType","name":"scala.collection.immutable.List","typedName":"scala.collection.immutable.List[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.NormalOption","typedName":"co.blocke.scala_reflection.models.NormalOption","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"a","fieldType":{"rtype":"ScalaOptionRType","name":"scala.Option","typedName":"scala.Option[scala.Int]","typeParamSymbols":["A"],"optionParamType":{"rtype":"IntRType","name":"scala.Int"}},"originalSymbol":null,"annotations":{}},{"name":"b","fieldType":{"rtype":"StringRType","name":"java.lang.String"},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}]},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":true,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }

  test("Scala Arrays") {
    val result = RType.of[WithScalaArray]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.WithScalaArray:
      |   fields ->
      |      list: Array of Array of Char
      |      x1: Array of Boolean
      |      x2: Array of Byte
      |      x3: Array of Char
      |      x4: Array of Double
      |      x5: Array of Float
      |      x6: Array of Int
      |      x7: Array of Long
      |      x8: Array of Short
      |      x9: Array of String
      |""".stripMargin
    )
    assertEquals(
      RType.ofJS[WithScalaArray],
      """{"rtype":"ScalaClassRType","name":"co.blocke.scala_reflection.models.WithScalaArray","typedName":"co.blocke.scala_reflection.models.WithScalaArray","typeParamSymbols":[],"typeParamValues":[],"typeMembers":[],"fields":[{"name":"list","fieldType":{"rtype":"ArrayRType","name":"[[C","typedName":"[[C[[C[scala.Char]]","typeParamSymbols":["A"],"elementType":{"rtype":"ArrayRType","name":"[C","typedName":"[C[scala.Char]","typeParamSymbols":["A"],"elementType":{"rtype":"CharRType","name":"scala.Char"}}},"originalSymbol":null,"annotations":{}},{"name":"x1","fieldType":{"rtype":"ArrayRType","name":"[Z","typedName":"[Z[scala.Boolean]","typeParamSymbols":["A"],"elementType":{"rtype":"BooleanRType","name":"scala.Boolean"}},"originalSymbol":null,"annotations":{}},{"name":"x2","fieldType":{"rtype":"ArrayRType","name":"[B","typedName":"[B[scala.Byte]","typeParamSymbols":["A"],"elementType":{"rtype":"ByteRType","name":"scala.Byte"}},"originalSymbol":null,"annotations":{}},{"name":"x3","fieldType":{"rtype":"ArrayRType","name":"[C","typedName":"[C[scala.Char]","typeParamSymbols":["A"],"elementType":{"rtype":"CharRType","name":"scala.Char"}},"originalSymbol":null,"annotations":{}},{"name":"x4","fieldType":{"rtype":"ArrayRType","name":"[D","typedName":"[D[scala.Double]","typeParamSymbols":["A"],"elementType":{"rtype":"DoubleRType","name":"scala.Double"}},"originalSymbol":null,"annotations":{}},{"name":"x5","fieldType":{"rtype":"ArrayRType","name":"[F","typedName":"[F[scala.Float]","typeParamSymbols":["A"],"elementType":{"rtype":"FloatRType","name":"scala.Float"}},"originalSymbol":null,"annotations":{}},{"name":"x6","fieldType":{"rtype":"ArrayRType","name":"[I","typedName":"[I[scala.Int]","typeParamSymbols":["A"],"elementType":{"rtype":"IntRType","name":"scala.Int"}},"originalSymbol":null,"annotations":{}},{"name":"x7","fieldType":{"rtype":"ArrayRType","name":"[J","typedName":"[J[scala.Long]","typeParamSymbols":["A"],"elementType":{"rtype":"LongRType","name":"scala.Long"}},"originalSymbol":null,"annotations":{}},{"name":"x8","fieldType":{"rtype":"ArrayRType","name":"[S","typedName":"[S[scala.Short]","typeParamSymbols":["A"],"elementType":{"rtype":"ShortRType","name":"scala.Short"}},"originalSymbol":null,"annotations":{}},{"name":"x9","fieldType":{"rtype":"ArrayRType","name":"[Ljava.lang.String;","typedName":"[Ljava.lang.String;[java.lang.String]","typeParamSymbols":["A"],"elementType":{"rtype":"StringRType","name":"java.lang.String"}},"originalSymbol":null,"annotations":{}}],"annotations":{},"mixins":["java.lang.Object","scala.Product","java.io.Serializable"],"isAppliedType":false,"isValueClass":false,"isCaseClass":true,"isAbstractClass":false,"nonConstructorFields":[],"sealedChildren":[],"childrenAreObject":false}"""
    )
  }
