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
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |   fields ->
      |      a: [U] Float
      |      b: [Q] Int
      |""".stripMargin)
  }

  /*  TODO: Let's try to kill these... They should be tested in Option, Either, ... respectively!
  test("0-level Option substitution") {
    val result = RType.of[Option[WithDefault]].asInstanceOf[ScalaOptionInfo]
    assertEquals( result.show(), """Option of ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |""".stripMargin)
  }

  test("0-level Either substitution") {
    val result = RType.of[Either[Int,WithDefault]].asInstanceOf[EitherInfo]
    assertEquals( result.show(), """Either:
    |   left--scala.Int
    |   right--ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |      fields:
    |         (0) a: scala.Int
    |         (1) b: java.lang.String
    |""".stripMargin)
  }

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

  test("0-level List (Seq) substitution") {
    val result = RType.of[List[WithDefault]].asInstanceOf[SeqLikeInfo]
    assertEquals( result.show(), """SeqLikeInfo(scala.collection.immutable.List): ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |""".stripMargin)
  }

  test("0-level Try substitution") {
    val result = RType.of[scala.util.Try[WithDefault]].asInstanceOf[TryInfo]
    assertEquals( result.show(), """Try of ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |   fields:
    |      (0) a: scala.Int
    |      (1) b: java.lang.String
    |""".stripMargin)
  }

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
  
  test("0-level Tuple substitution") {
    val result = RType.of[(Int,Boolean)].asInstanceOf[TupleInfo]
    assertEquals( result.show(), """(
    |   scala.Int
    |   scala.Boolean
    |)
    |""".stripMargin)
  }

  test("0-level Union substitution") {
    val result = RType.of[String | WithDefault].asInstanceOf[UnionInfo]
    assertEquals( result.show(), """Union:
    |   left--java.lang.String
    |   right--ScalaCaseClassInfo(co.blocke.scala_reflection.WithDefault):
    |      fields:
    |         (0) a: scala.Int
    |         (1) b: java.lang.String
    |""".stripMargin)
  }

  test("0-level Intersection substitution") {    
    val result = RType.of[Stackable[Int] & Floatable[String]].asInstanceOf[IntersectionInfo]
    assertEquals( result.show(), """Intersection:
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
    assertEquals( result.prettyPrint(), """co.blocke.scala_reflection.models.DuoHolder:
      |   fields ->
      |      x: co.blocke.scala_reflection.models.DuoTypes[Q,U]:
      |         fields ->
      |            a: [U] Float
      |            b: [Q] Int
      |""".stripMargin)
  }

  /*
  test("2nd level param substitution - Option") {
    val result = RType.of[OptHolder].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.OptHolder):
    |   fields:
    |      (0) a: Option of ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |         fields:
    |            (0)[Q] a: java.lang.String
    |            (1)[U] b: scala.Boolean
    |""".stripMargin)
  }

  test("3rd level param substitution - Option") {
    val result = RType.of[OptHolder2].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.OptHolder2):
    |   fields:
    |      (0) a: Option of Option of ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |         fields:
    |            (0)[Q] a: java.lang.String
    |            (1)[U] b: scala.Boolean
    |""".stripMargin)
  }

  test("2nd and 3rd level param substitution - Either") {
    val result = RType.of[EitherHolder].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.EitherHolder):
    |   fields:
    |      (0) a: Either:
    |         left--ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |            fields:
    |               (0)[Q] a: scala.Int
    |               (1)[U] b: scala.Float
    |         right--Option of ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |            fields:
    |               (0)[Q] a: java.lang.String
    |               (1)[U] b: scala.Boolean
    |""".stripMargin)
  }

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

  test("2nd level subsitution in a class field") {
    val result = RType.of[DuoClass].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.DuoClass):
    |   fields:
    |      (0) a: ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |         fields:
    |            (0)[Q] a: scala.Int
    |            (1)[U] b: ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes) (self-ref recursion)
    |""".stripMargin)
  }

  test("List and Map subsitituion") {
    val result = RType.of[ListMapSub].asInstanceOf[ScalaCaseClassInfo]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.ListMapSub):
    |   fields:
    |      (0) a: SeqLikeInfo(scala.collection.immutable.List): ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |         fields:
    |            (0)[Q] a: scala.Int
    |            (1)[U] b: scala.Byte
    |      (1) b: MapLikeInfo(scala.collection.immutable.Map):
    |         java.lang.String
    |         ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |            fields:
    |               (0)[Q] a: scala.Float
    |               (1)[U] b: scala.Short
    |""".stripMargin)
  }

  test("Try type substitution") {
    val result = RType.of[TryHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.TryHolder):
    |   fields:
    |      (0) a: Try of ScalaCaseClassInfo(co.blocke.scala_reflection.DuoTypes):
    |         fields:
    |            (0)[Q] a: java.lang.String
    |            (1)[U] b: scala.Int
    |""".stripMargin)
  }

  test("Trait type substitution") {
    val result = RType.of[TypeShellHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.TypeShellHolder):
    |   fields:
    |      (0) a: TraitInfo(co.blocke.scala_reflection.TypeShell) actualParamTypes: [
    |            X: scala.Int
    |         ] with fields:
    |            x[X]: scala.Int
    |""".stripMargin)
  }

  test("Union type substitution") {
    val result = RType.of[UnionHolder]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.UnionHolder):
    |   fields:
    |      (0) a: Union:
    |         left--scala.Int
    |         right--TraitInfo(co.blocke.scala_reflection.TypeShell) actualParamTypes: [
    |               X: java.lang.String
    |            ] with fields:
    |               x[X]: java.lang.String
    |""".stripMargin)
  }

  test("Type member substitutions") {
    val result = RType.of[Envelope[FancyBody,Boolean]]
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.Envelope):
    |   fields:
    |      (0) id: java.lang.String
    |      (1)[T] body: ScalaCaseClassInfo(co.blocke.scala_reflection.FancyBody):
    |         fields:
    |            (0) message: java.lang.String
    |   type members:
    |      Giraffe[T]: ScalaCaseClassInfo(co.blocke.scala_reflection.FancyBody):
    |         fields:
    |            (0) message: java.lang.String
    |""".stripMargin)
  }

  test("Nested trait substitutions") {
    val inst: T10[T11[Int, T5[Double, Char]], String] = TFoo6(TBlah1(5, TBar7(1.2, 'Z')), "wow")
    val result = RType.inTermsOf[T10[T11[Int, T5[Double, Char]], String]]( inst.getClass )
    assertEquals( result.show(), """ScalaCaseClassInfo(co.blocke.scala_reflection.TFoo6):
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