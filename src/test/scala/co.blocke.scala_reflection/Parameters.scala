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

  test("Nested trait substitutions (inTermsOf)") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    val rt = RType.of[T10[T11[Int, T5[Double, Char]], String]].asInstanceOf[TraitRType[?]]
    val result = RType.inTermsOf[T10[T11[Int, T5[Double, Char]], String]](inst.getClass)
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.TFoo6[Char,String,Int,Double]:
      |   fields ->
      |      x: co.blocke.scala_reflection.models.T11[Int,T5[Double,Char]] (trait):
      |         fields ->
      |            w: [W] Int
      |            z: [Z] co.blocke.scala_reflection.models.T5[Double,Char] (trait):
      |               fields ->
      |                  thing1: [X] Double
      |                  thing2: [Y] Char
      |      y: [B] String
      |""".stripMargin
    )
  }

  test("With nested Option and List (inTermsOf)") {
    val inst: Base[Level1[String, Boolean], Int] = BaseClass(L1Class("foo", Some(List(true))), 3)
    val result = RType.inTermsOf[Base[Level1[String, Boolean], Int]](
      Class.forName("co.blocke.scala_reflection.models.BaseClass")
    )
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.BaseClass[String,Int,Boolean]:
      |   fields ->
      |      a: co.blocke.scala_reflection.models.Level1[String,Boolean] (trait):
      |         fields ->
      |            t: [T] String
      |            u: Option of List of Boolean
      |      b: [Y] Int
      |""".stripMargin
    )
  }

  test("With nested Try (inTermsOf)") {
    val result = RType.inTermsOf[TryIt[Int, Double]](Class.forName("co.blocke.scala_reflection.models.TryItC"))
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.TryItC[Int,Double]:
      |   fields ->
      |      x: Try of Int
      |      y: Try of Option of Double
      |""".stripMargin
    )
  }

  test("With nested Map and Array (inTermsOf)") {
    val result =
      RType.inTermsOf[MapIt[Int, Double, String, Boolean]](Class.forName("co.blocke.scala_reflection.models.MapItC"))
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.MapItC[Int,Double,String,Boolean]:
      |   fields ->
      |      x: Map of:
      |         key: Int
      |         value: Option of Double
      |      s: Array of String
      |      t: Array of List of Boolean
      |""".stripMargin
    )
  }

  test("With nested case class and non-case class (inTermsOf)") {
    val result =
      RType.inTermsOf[ClassistBase[Int, Short]](Class.forName("co.blocke.scala_reflection.models.ClassistC"))
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ClassistC[Int,Short]:
      |   fields ->
      |      t: co.blocke.scala_reflection.models.CClass[CClassLevel2[Int]]:
      |         fields ->
      |            x: List of co.blocke.scala_reflection.models.CClassLevel2[Int]:
      |               fields ->
      |                  z: [Z] Int
      |      u: co.blocke.scala_reflection.models.PClass[Short]:
      |         fields ->
      |            y: List of Short
      |""".stripMargin
    )
  }

  test("With nested case class and non-case class (inverted) (inTermsOf)") {
    val result =
      RType.inTermsOf[ClassistBaseInv[Int, Short]](Class.forName("co.blocke.scala_reflection.models.ClassistCInv"))
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.ClassistCInv[Int,Short]:
      |   fields ->
      |      t: co.blocke.scala_reflection.models.CClass[Int]:
      |         fields ->
      |            x: List of Int
      |      u: co.blocke.scala_reflection.models.PClass[Short]:
      |         fields ->
      |            y: List of Short
      |""".stripMargin
    )
  }

  test("InTermsOf deep type substitution (inTermsOf)") {
    val result =
      RType.inTermsOf[Basis[List[Option[Int | Boolean]]]](Class.forName("co.blocke.scala_reflection.models.Thingy2"))
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.Thingy2[List[Option[Int | Boolean]]]:
      |   fields ->
      |      a: Int
      |      b: String
      |      c: [T] List of Option of Union of:
      |         left--Int
      |         right--Boolean
      |""".stripMargin
    )
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

  test("Complex type parameter relationships work") {
    val result = RType.of[PersonX[Artist[Int, Hobby[String, Double]], Boolean]]
    assertEquals(
      result.pretty,
      """co.blocke.scala_reflection.models.PersonX[Artist[Int,Hobby[String,Double]],Boolean] (sealed trait):
      |   fields ->
      |      who: [X] co.blocke.scala_reflection.models.Artist[Int,Hobby[String,Double]] (sealed trait):
      |         fields ->
      |            instrument: [W] Int
      |            effort: [Z] co.blocke.scala_reflection.models.Hobby[String,Double] (sealed trait):
      |               fields ->
      |                  thing1: [X] String
      |                  thing2: [Y] Double
      |               children ->
      |                  co.blocke.scala_reflection.models.Sports[String,Double]:
      |                     fields ->
      |                        thing1: [A] String
      |                        thing2: [B] Double
      |         children ->
      |            co.blocke.scala_reflection.models.Painter[Int,String]:
      |               fields ->
      |                  instrument: [A] Int
      |                  effort: [B] String
      |      org: [Y] Boolean
      |   children ->
      |      co.blocke.scala_reflection.models.EmployeeX[Double,Boolean,Int,String]:
      |         fields ->
      |            who: co.blocke.scala_reflection.models.Artist[Int,Hobby[String,Double]] (sealed trait):
      |               fields ->
      |                  instrument: [W] Int
      |                  effort: [Z] co.blocke.scala_reflection.models.Hobby[String,Double] (sealed trait):
      |                     fields ->
      |                        thing1: [X] String
      |                        thing2: [Y] Double
      |                     children ->
      |                        co.blocke.scala_reflection.models.Sports[String,Double]:
      |                           fields ->
      |                              thing1: [A] String
      |                              thing2: [B] Double
      |               children ->
      |                  co.blocke.scala_reflection.models.Painter[Int,String]:
      |                     fields ->
      |                        instrument: [A] Int
      |                        effort: [B] String
      |            org: [B] Boolean
      |""".stripMargin
    )
  }
