package co.blocke.scala_reflection

import munit.*
import rtypes.*
import models.*

class Parameters extends munit.FunSuite:

  test("0-level param substitution") {
    val result = RType.of[DuoTypes[Int, Float]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.DuoTypes[Int,Float]:
      |   fields ->
      |      a: [U] Float
      |      b: [Q] Int
      |""".stripMargin
    )
  }

  test("0-level Option substitution") {
    val result = RType.of[Option[WithDefault]]
    assertEquals(
      result.pretty,
      """Option of co.blocke.scala_reflection.models.WithDefault:
      |   fields ->
      |      a: Int
      |      b: String (default value: wow)
      |""".stripMargin
    )
  }

  test("0-level Either substitution") {
    val result = RType.of[Either[Int, WithDefault]]
    assertEquals(
      result.pretty,
      """Either of:
      |   left--Int
      |   right--co.blocke.scala_reflection.models.WithDefault:
      |         fields ->
      |            a: Int
      |            b: String (default value: wow)
      |""".stripMargin
    )
  }

  test("0-level Map substitution") {
    val result = RType.of[Map[Int, WithDefault]]
    assertEquals(
      result.pretty,
      """Map of:
      |   key: Int
      |   value: co.blocke.scala_reflection.models.WithDefault:
      |      fields ->
      |         a: Int
      |         b: String (default value: wow)
      |""".stripMargin
    )
  }

  test("0-level List (Seq) substitution") {
    val result = RType.of[List[WithDefault]]
    assertEquals(
      result.pretty,
      """List of co.blocke.scala_reflection.models.WithDefault:
      |   fields ->
      |      a: Int
      |      b: String (default value: wow)
      |""".stripMargin
    )
  }

  test("0-level Try substitution") {
    val result = RType.of[scala.util.Try[WithDefault]]
    assertEquals(
      result.pretty,
      """Try of co.blocke.scala_reflection.models.WithDefault:
      |   fields ->
      |      a: Int
      |      b: String (default value: wow)
      |""".stripMargin
    )
  }

  test("0-level Trait substitution") {
    val result = RType.of[ParamThing[WithDefault]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ParamThing[WithDefault] (trait):
      |   fields ->
      |      id: [X] co.blocke.scala_reflection.models.WithDefault:
      |         fields ->
      |            a: Int
      |            b: String (default value: wow)
      |""".stripMargin
    )
  }

  test("0-level Tuple substitution") {
    val result = RType.of[(Int, Boolean)]
    assertEquals(
      result.pretty,
      """Tuple of:
      |   0: Int
      |   1: Boolean
      |""".stripMargin
    )
  }

  test("0-level Union substitution") {
    val result = RType.of[String | WithDefault]
    assertEquals(
      result.pretty,
      """Union of:
      |   left--String
      |   right--co.blocke.scala_reflection.models.WithDefault:
      |         fields ->
      |            a: Int
      |            b: String (default value: wow)
      |""".stripMargin
    )
  }

  test("0-level Intersection substitution") {
    val result = RType.of[Stackable[Int] & Floatable[String]]
    assertEquals(
      result.pretty,
      """Intersection of:
      |   left--co.blocke.scala_reflection.models.Stackable[Int] (trait):
      |         fields ->
      |   right--co.blocke.scala_reflection.models.Floatable[String] (trait):
      |         fields ->
      |""".stripMargin
    )
  }

  test("1st level param substitution") {
    val result = RType.of[DuoHolder]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.DuoHolder:
      |   fields ->
      |      x: co.blocke.scala_reflection.models.DuoTypes[Int,Float]:
      |         fields ->
      |            a: [U] Float
      |            b: [Q] Int
      |""".stripMargin
    )
  }

  test("2nd level subsitution in a class field") {
    val result = RType.of[DuoTypes[Int, Thingy[Boolean]]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.DuoTypes[Int,Thingy[Boolean]]:
      |   fields ->
      |      a: [U] co.blocke.scala_reflection.models.Thingy[Boolean]:
      |         fields ->
      |            name: String
      |            payload: [Z] Boolean
      |      b: [Q] Int
      |""".stripMargin
    )
  }

  test("2nd level subsitution in a class field, with self-reference") {
    val result = RType.of[DuoTypes[Int, DuoTypes[String, Thingy[Boolean]]]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.DuoTypes[Int,DuoTypes[String,Thingy[Boolean]]]:
      |   fields ->
      |      a: [U] co.blocke.scala_reflection.models.DuoTypes[String,Thingy[Boolean]]:
      |         fields ->
      |            a: [U] co.blocke.scala_reflection.models.Thingy[Boolean]:
      |               fields ->
      |                  name: String
      |                  payload: [Z] Boolean
      |            b: [Q] String
      |      b: [Q] Int
      |""".stripMargin
    )
  }

  test("2nd level param substitution - Option") {
    val result = RType.of[OptHolder]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.OptHolder:
      |   fields ->
      |      a: Option of co.blocke.scala_reflection.models.DuoTypes[String,Boolean]:
      |         fields ->
      |            a: [U] Boolean
      |            b: [Q] String
      |""".stripMargin
    )
  }

  test("3rd level param substitution - Option") {
    val result = RType.of[OptHolder2]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.OptHolder2:
      |   fields ->
      |      a: Option of Option of co.blocke.scala_reflection.models.DuoTypes[String,Boolean]:
      |         fields ->
      |            a: [U] Boolean
      |            b: [Q] String
      |""".stripMargin
    )
  }

  test("2nd and 3rd level param substitution - Either") {
    val result = RType.of[EitherHolder]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.EitherHolder:
      |   fields ->
      |      a: Either of:
      |         left--co.blocke.scala_reflection.models.DuoTypes[Int,Float]:
      |               fields ->
      |                  a: [U] Float
      |                  b: [Q] Int
      |         right--Option of co.blocke.scala_reflection.models.DuoTypes[String,Boolean]:
      |               fields ->
      |                  a: [U] Boolean
      |                  b: [Q] String
      |""".stripMargin
    )
  }

  test("Opaque type alias type substitution (rare)") {
    val result = RType.of[AliasTypeSub]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.AliasTypeSub:
      |   fields ->
      |      a: alias mystery defined as co.blocke.scala_reflection.models.DuoTypes[Byte,Short]:
      |         fields ->
      |            a: [U] Short
      |            b: [Q] Byte
      |""".stripMargin
    )
  }

  test("2nd level subsitution in a class field") {
    val result = RType.of[DuoClass]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.DuoClass:
      |   fields ->
      |      a: co.blocke.scala_reflection.models.DuoTypes[Int,DuoTypes[Byte,Short]]:
      |         fields ->
      |            a: [U] co.blocke.scala_reflection.models.DuoTypes[Byte,Short]:
      |               fields ->
      |                  a: [U] Short
      |                  b: [Q] Byte
      |            b: [Q] Int
      |""".stripMargin
    )
  }

  test("List and Map subsitituion") {
    val result = RType.of[ListMapSub]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ListMapSub:
      |   fields ->
      |      a: List of co.blocke.scala_reflection.models.DuoTypes[Int,Byte]:
      |         fields ->
      |            a: [U] Byte
      |            b: [Q] Int
      |      b: Map of:
      |         key: String
      |         value: co.blocke.scala_reflection.models.DuoTypes[Float,Short]:
      |            fields ->
      |               a: [U] Short
      |               b: [Q] Float
      |""".stripMargin
    )
  }

  test("Try type substitution") {
    val result = RType.of[TryHolder]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.TryHolder:
      |   fields ->
      |      a: Try of co.blocke.scala_reflection.models.DuoTypes[String,Int]:
      |         fields ->
      |            a: [U] Int
      |            b: [Q] String
    |""".stripMargin
    )
  }

  test("Trait type substitution") {
    val result = RType.of[TypeShellHolder]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.TypeShellHolder:
      |   fields ->
      |      a: co.blocke.scala_reflection.models.TypeShell[Int] (trait):
      |         fields ->
      |            x: [X] Int
      |""".stripMargin
    )
  }

  test("Union type substitution") {
    val result = RType.of[UnionHolder]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.UnionHolder:
      |   fields ->
      |      a: Union of:
      |         left--Int
      |         right--co.blocke.scala_reflection.models.TypeShell[String] (trait):
      |               fields ->
      |                  x: [X] String
      |""".stripMargin
    )
  }

  test("Type member substitutions") {
    val result = RType.of[Envelope[FancyBody, Boolean]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Envelope[FancyBody,Boolean]:
      |   fields ->
      |      id: String
      |      body: [T] co.blocke.scala_reflection.models.FancyBody:
      |         fields ->
      |            message: String
      |   type members ->
      |      Giraffe: [T] co.blocke.scala_reflection.models.FancyBody (seen before, details above)
      |      Foo: Int
      |""".stripMargin
    )
  }

  test("Nested trait substitutions") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    val result = RType.mapTypesForSymbols[T10[T11[Int, T5[Double, Char]], String]](inst.getClass.getName)
    val r2 = result.map { case (k, v) => (k.toString, v.typedName.toString) }.toMap
    assertEquals(r2, Map("A" -> "scala.Char", "B" -> "java.lang.String", "C" -> "scala.Int", "D" -> "scala.Double"))
  }

  test("With nested Option and List") {
    val inst: Base[Level1[String, Boolean], Int] = BaseClass(L1Class("foo", Some(List(true))), 3)
    val result = RType.mapTypesForSymbols[Base[Level1[String, Boolean], Int]](
      "co.blocke.scala_reflection.models.BaseClass"
    )
    val r2 = result.map { case (k, v) => (k.toString, v.typedName.toString) }.toMap
    assertEquals(r2, Map("X" -> "java.lang.String", "Y" -> "scala.Int", "Z" -> "scala.Boolean"))
  }

  test("With nested Try") {
    val result = RType.mapTypesForSymbols[TryIt[Int, Double]]("co.blocke.scala_reflection.models.TryItC")
    val r2 = result.map { case (k, v) => (k.toString, v.typedName.toString) }.toMap
    assertEquals(r2, Map("A" -> "scala.Int", "B" -> "scala.Double"))
  }

  test("With nested Map and Array") {
    val result =
      RType.mapTypesForSymbols[MapIt[Int, Double, String, Boolean]]("co.blocke.scala_reflection.models.MapItC")
    val r2 = result.map { case (k, v) => (k.toString, v.typedName.toString) }.toMap
    assertEquals(r2, Map("A" -> "scala.Int", "B" -> "scala.Double", "W" -> "java.lang.String", "U" -> "scala.Boolean"))
  }

  test("With nested case class and non-case class") {
    val result =
      RType.mapTypesForSymbols[ClassistBase[Int, Short]]("co.blocke.scala_reflection.models.ClassistC")
    val r2 = result.map { case (k, v) => (k.toString, v.typedName.toString) }.toMap
    assertEquals(r2, Map("A" -> "scala.Int", "B" -> "scala.Short"))
  }

  test("With nested case class and non-case class (inverted)") {
    val result =
      RType.mapTypesForSymbols[ClassistBaseInv[Int, Short]]("co.blocke.scala_reflection.models.ClassistCInv")
    val r2 = result.map { case (k, v) => (k.toString, v.typedName.toString) }.toMap
    assertEquals(r2, Map("A" -> "scala.Int", "B" -> "scala.Short"))
  }

  test("InTermsOf deep type substitution") {
    val result =
      RType.mapTypesForSymbols[Basis[List[Option[Int | Boolean]]]]("co.blocke.scala_reflection.models.Thingy2")
    val r2 = result.map { case (k, v) => (k.toString, v.typedName.toString) }.toMap
    assertEquals(r2, Map("T" -> "scala.collection.immutable.List[scala.Option[scala.Union[scala.Int,scala.Boolean]]]"))
  }

  test("Parameterized class defined inside an object") {
    val result = RType.of[Outside.Blah[Char]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Outside$Blah[Char]:
      |   fields ->
      |      a: co.blocke.scala_reflection.models.Outside$PersonZ[Char]:
      |         fields ->
      |            name: String
      |            thing: [Z] Char
      |""".stripMargin
    )
  }
