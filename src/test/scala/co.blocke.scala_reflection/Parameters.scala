package co.blocke.scala_reflection

import munit.*
import rtypes.*
import rtypes.PrimitiveRTypes.*
import models.*
// import java.util.Optional
// import scala.util.Try

class Parameters extends munit.FunSuite:

  test("0-level param substitution") {
    val result = RType.of[DuoTypes[Int,Float]]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |   fields ->
      |      a: [U] Float
      |      b: [Q] Int
      |""".stripMargin)
  }

  test("0-level Option substitution") {
    val result = RType.of[Option[WithDefault]]
    assertEquals( result.pretty(), """Option of co.blocke.scala_reflection.models.WithDefault:
      |   fields ->
      |      a: Int
      |      b: String (default value: wow)
      |""".stripMargin)
  }

  test("0-level Either substitution") {
    val result = RType.of[Either[Int,WithDefault]]
    assertEquals( result.pretty(), """Either of:
      |   left--Int
      |   right--co.blocke.scala_reflection.models.WithDefault:
      |         fields ->
      |            a: Int
      |            b: String (default value: wow)
      |""".stripMargin)
  }

  /*  TODO: Let's try to kill these... They should be tested in Option, Either, ... respectively!
  test("0-level Map substitution") {
    val result = RType.of[Map[Int,WithDefault]].asInstanceOf[MapLikeInfo]
    assertEquals( result.show(), """MapLikeInfo(scala.collection.immutable.Map):
    |   scala.Int
    |   ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |      fields:
    |         (0) a: scala.Int
    |         (1) b: java.lang.String
    |""".stripMargin)
  }
  */

  test("0-level List (Seq) substitution") {
    val result = RType.of[List[WithDefault]]
    assertEquals( result.pretty(), """List of: co.blocke.scala_reflection.models.WithDefault:
      |   fields ->
      |      a: Int
      |      b: String (default value: wow)
      |""".stripMargin)
  }

  test("0-level Try substitution") {
    val result = RType.of[scala.util.Try[WithDefault]]
    assertEquals( result.pretty(), """Try of co.blocke.scala_reflection.models.WithDefault:
      |   fields ->
      |      a: Int
      |      b: String (default value: wow)
      |""".stripMargin)
  }

  /*
  test("0-level Trait substitution") {
    val result = RType.of[ParamThing[WithDefault]].asInstanceOf[TraitInfo]
    assertEquals( result.show(), """TraitInfo(co.blocke.scala_reflection.ParamThing) actualParamTypes: [
    |   X: ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |      fields:
    |         (0) a: scala.Int
    |         (1) b: java.lang.String
    |] with fields:
    |   id[X]: ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |""".stripMargin)
  }
 */ 

  test("0-level Tuple substitution") {
    val result = RType.of[(Int,Boolean)].asInstanceOf[TupleRType[_]]
    assertEquals( result.pretty(), """Tuple of:
      |   0: Int
      |   1: Boolean
      |""".stripMargin)
  }

  test("0-level Union substitution") {
    val result = RType.of[String | WithDefault]
    assertEquals( result.pretty(), """Union of:
      |   left--String
      |   right--co.blocke.scala_reflection.models.WithDefault:
      |         fields ->
      |            a: Int
      |            b: String (default value: wow)
      |""".stripMargin)
  }

/* need trait support for this test
  test("0-level Intersection substitution") {    
    val result = RType.of[Stackable[Int] & Floatable[String]]
    assertEquals( result.pretty(), """Intersection:
    |   left--TraitInfo(co.blocke.scala_reflection.Stackable) actualParamTypes: [
    |         T: scala.Int
    |      ] with fields:
    |   right--TraitInfo(co.blocke.scala_reflection.Floatable) actualParamTypes: [
    |         U: java.lang.String
    |      ] with fields:
    |""".stripMargin)
  }
  */

  test("1st level param substitution") {
    val result = RType.of[DuoHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.DuoHolder:
      |   fields ->
      |      x: co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] Float
      |            b: [Q] Int
      |""".stripMargin)
  }

  test("2nd level subsitution in a class field") {
    val result = RType.of[DuoTypes[Int,Thingy[Boolean]]]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |   fields ->
      |      a: [U] co.blocke.scala_reflection.models.Thingy[Z]:
      |         fields ->
      |            name: String
      |            payload: [Z] Boolean
      |      b: [Q] Int
      |""".stripMargin)
  }

  test("2nd level subsitution in a class field, with self-reference") {
    val result = RType.of[DuoTypes[Int,DuoTypes[String,Thingy[Boolean]]]]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |   fields ->
      |      a: [U] co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] co.blocke.scala_reflection.models.Thingy[Z]:
      |               fields ->
      |                  name: String
      |                  payload: [Z] Boolean
      |            b: [Q] String
      |      b: [Q] Int
      |""".stripMargin)
  }

  test("2nd level param substitution - Option") {
    val result = RType.of[OptHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.OptHolder:
      |   fields ->
      |      a: Option of co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] Boolean
      |            b: [Q] String
      |""".stripMargin)
  }

  test("3rd level param substitution - Option") {
    val result = RType.of[OptHolder2]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.OptHolder2:
      |   fields ->
      |      a: Option of Option of co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] Boolean
      |            b: [Q] String
      |""".stripMargin)
  }

  test("2nd and 3rd level param substitution - Either") {
    val result = RType.of[EitherHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.EitherHolder:
      |   fields ->
      |      a: Either of:
      |         left--co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |               fields ->
      |                  a: [U] Float
      |                  b: [Q] Int
      |         right--Option of co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |               fields ->
      |                  a: [U] Boolean
      |                  b: [Q] String
      |""".stripMargin)
  }

  /*
  test("Opaque type alias type substitution (rare)") {
    val result = RType.of[AliasTypeSub].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.AliasTypeSub):
    |   fields:
    |      (0) a: alias mystery defined as ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |         fields:
    |            (0)[Q] a: scala.Byte
    |            (1)[U] b: scala.Short
    |""".stripMargin)
  }
  */

  test("2nd level subsitution in a class field") {
    val result = RType.of[DuoClass]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.DuoClass:
      |   fields ->
      |      a: co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |               fields ->
      |                  a: [U] Short
      |                  b: [Q] Byte
      |            b: [Q] Int
      |""".stripMargin)
  }

  test("List and Map subsitituion") {
    val result = RType.of[ListMapSub]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.ListMapSub:
      |   fields ->
      |      a: List of: co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] Byte
      |            b: [Q] Int
      |      b: Map of:
      |         key: String
      |         value: co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |            fields ->
      |               a: [U] Short
      |               b: [Q] Float
      |""".stripMargin)
  }

  test("Try type substitution") {
    val result = RType.of[TryHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.TryHolder:
      |   fields ->
      |      a: Try of co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] Int
      |            b: [Q] String
    |""".stripMargin)
  }

  test("Trait type substitution") {
    val result = RType.of[TypeShellHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.TypeShellHolder:
      |   fields ->
      |      a: co.blocke.reflection.models.TypeShell (trait):
      |         fields ->
      |            x: [X] Int
      |""".stripMargin)
  }

  test("Union type substitution") {
    val result = RType.of[UnionHolder]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.UnionHolder:
      |   fields ->
      |      a: Union of:
      |         left--Int
      |         right--co.blocke.reflection.models.TypeShell (trait):
      |               fields ->
      |                  x: [X] String
      |""".stripMargin)
  }

  test("Type member substitutions") {
    val result = RType.of[Envelope[FancyBody,Boolean]]
    assertEquals( result.pretty(), """co.blocke.scala_reflection.models.Envelope[T,U]:
      |   fields ->
      |      id: String
      |      body: [T] co.blocke.scala_reflection.models.FancyBody:
      |         fields ->
      |            message: String
      |   type members ->
      |      Giraffe: [T] co.blocke.scala_reflection.models.FancyBody (seen before, details above)
      |      Foo: [Foo] Int
      |""".stripMargin)
  }

  /*
  test("Nested trait substitutions") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    val rt = RType.of[T10[T11[Int, T5[Double, Char]], String]]
    val result = RType.inTermsOf( rt, inst.getClass.getName )
    assertEquals( result.pretty(), """ScalaCaseClassInfo(co.blocke.scala_reflection.TFoo6):
    |   fields:
    |      (0) x: TraitInfo(co.blocke.scala_reflection.T11) actualParamTypes: [
    |            W: scala.Int
    |            Z: TraitInfo(co.blocke.scala_reflection.T5) actualParamTypes: [
    |                  X: scala.Double
    |                  Y: scala.Char
    |               ] with fields:
    |                  thing1[X]: scala.Double
    |                  thing2[Y]: scala.Char
    |         ] with fields:
    |            w[W]: scala.Int
    |            z[Z]: TraitInfo(co.blocke.scala_reflection.T5) actualParamTypes: [
    |               X: scala.Double
    |               Y: scala.Char
    |            ] with fields:
    |               thing1[X]: scala.Double
    |               thing2[Y]: scala.Char
    |      (1)[B] y: java.lang.String
    |""".stripMargin)
  }
  */

/*
  test("With nested Option and List") {
    val inst: Base[Level1[String,Boolean],Int] = BaseClass(L1Class("foo",Some(List(true))), 3)
    val result = RType.inTermsOf[Base[Level1[String,Boolean],Int]](inst.getClass)
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.BaseClass):
    |   fields:
    |      (0) a: TraitInfo(co.blocke.scala_reflection.Level1) actualParamTypes: [
    |            T: java.lang.String
    |            U: scala.Boolean
    |         ] with fields:
    |            t[T]: java.lang.String
    |            u: Option of SeqLikeInfo(scala.collection.immutable.List): scala.Boolean
    |      (1)[Y] b: scala.Int
    |""".stripMargin)
  }

  test("With nested Try") {
    val result = RType.inTermsOf[TryIt[Int,Double]](Class.forName("co.blocke.scala_reflection.TryItC"))
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.TryItC):
    |   fields:
    |      (0) x: Try of scala.Int
    |      (1) y: Try of Option of scala.Double
    |""".stripMargin)
  }

  test("With nested Map and Array") {
    val result = RType.inTermsOf[MapIt[Int,Double,String,Boolean]](Class.forName("co.blocke.scala_reflection.MapItC"))
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.MapItC):
    |   fields:
    |      (0) x: MapLikeInfo(scala.collection.immutable.Map):
    |         scala.Int
    |         Option of scala.Double
    |      (1) s: array of java.lang.String
    |      (2) t: array of SeqLikeInfo(scala.collection.immutable.List): scala.Boolean
    |""".stripMargin)
  }

  test("With nested case class and non-case class") {
    val result = RType.inTermsOf[ClassistBase[Int,Short]](Class.forName("co.blocke.scala_reflection.ClassistC"))
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.ClassistC):
    |   fields:
    |      (0) t: ScalaCaseClassInfo(co.blocke.scala_reflection.CClass):
    |         fields:
    |            (0) x: SeqLikeInfo(scala.collection.immutable.List): ScalaCaseClassInfo(co.blocke.scala_reflection.CClassLevel2):
    |               fields:
    |                  (0)[Z] z: scala.Int
    |      (1) u: ScalaClassInfo(co.blocke.scala_reflection.PClass):
    |         fields:
    |            (0) y: SeqLikeInfo(scala.collection.immutable.List): scala.Short
    |         non-constructor fields:
    |""".stripMargin)
  }

  test("With nested case class and non-case class (inverted)") {
    val result = RType.inTermsOf[ClassistBaseInv[Int,Short]](Class.forName("co.blocke.scala_reflection.ClassistCInv"))
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.ClassistCInv):
    |   fields:
    |      (0) t: ScalaCaseClassInfo(co.blocke.scala_reflection.CClass):
    |         fields:
    |            (0) x: SeqLikeInfo(scala.collection.immutable.List): scala.Int
    |      (1) u: ScalaClassInfo(co.blocke.scala_reflection.PClass):
    |         fields:
    |            (0) y: SeqLikeInfo(scala.collection.immutable.List): scala.Short
    |         non-constructor fields:
    |""".stripMargin)
  }

  test("InTermsOf deep type substitution") {
    val result = RType.inTermsOf[Basis[List[Option[Int|Boolean]]]](Class.forName("co.blocke.scala_reflection.Thingy"))
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Thingy):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |      (2)[T] c: SeqLikeInfo(scala.collection.immutable.List): Option of Union:
    |         left--scala.Int
    |         right--scala.Boolean
    |""".stripMargin)
  }
  */