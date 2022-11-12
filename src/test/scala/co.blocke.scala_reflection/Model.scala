package co.blocke.scala_reflection

import co.blocke.reflect.*
import scala.util.Try

// Basic Tasty class
case class Person(name: String, age: Int, other: Int | Boolean)

// Match / dependent types
type Elem[X] = X match {
  case String => Char
  case Array[t] => t
  case Iterable[t] => t
}
case class Definitely( id: Elem[List[Int]], stuff: Elem[String] )

case class ScalaPrimitives(
  a: Boolean,
  b: Byte,
  c: Char,
  d: Double,
  e: Float,
  f: Int,
  g: Long,
  h: Short,
  i: String,
  j: Any
)

// Mixin tests
trait SJCapture {
  var captured: java.util.HashMap[String, _] =
    new java.util.HashMap[String, Any]()
}
class SJCaptureJava extends SJCapture

case class WithMix(id:String) extends SJCapture

// Object/field Annotations
@ClassAnno(name="Foom")
case class WithAnnotation(@FieldAnno(idx=5) id: String)

@Skip_Reflection
case class SkipMe(a: Int, b: String)

// Opaque type aliases
opaque type EMP_ID = Int
case class Employee(eId: EMP_ID, age: Int)

// Value classes
case class IdUser(id: Int) extends AnyVal  // value class
case class Employee2(eId: IdUser, age: Int)

// Parameterized classes
case class WithParam[T,U](one:T, two:U)

// Opaque type is union
opaque type GEN_ID = Int | String
case class OpaqueUnion(id: GEN_ID)

// With default value
case class WithDefault(a: Int, b: String = "wow")

// Either
case class BothSides(a: scala.util.Either[Int,String])
case class BothSidesWithOption(a: scala.util.Either[Int, Option[String]])
case class BothSidesWithUnion(a: scala.util.Either[Int, String|Boolean])
case class BothSidesParam[Z](a: scala.util.Either[Int, Option[ParamOption[Z]]])

// Options
case class NormalOption(a: Option[Int])
case class NestedOption(a: Option[Option[Int]])
case class ParamOption[T](a: Option[T])
case class UnionHavingOption(a: Boolean | Option[Int], b: Boolean | java.util.Optional[Int])
case class OptionHavingUnion(a: Option[Boolean|String])

// Plain class
class PlainGood(val a: Int, val b: String)
class PlainNonVal(val a: Int, b: String)

// Collections - immutable
case class Coll1(a: List[String])
case class Coll2(a: scala.collection.immutable.HashSet[String])
case class Coll3(a: Map[String,Float])
case class Coll4(a: scala.collection.immutable.ListMap[String,Boolean])
// Collections - mutable
case class Coll1m(a: scala.collection.mutable.ListBuffer[String])
case class Coll2m(a: scala.collection.mutable.HashSet[String])
case class Coll3m(a: scala.collection.mutable.Map[String,Float])
case class Coll4m(a: scala.collection.mutable.HashMap[String,Boolean])
case class NestedColl(a: Map[String, List[Option[Int]]])

// Tuple
case class TupleTurtle[Z]( t: (Int, Z, List[String], NormalOption))

// Scala 2.x style Enumeration
object WeekDay extends Enumeration {
  type WeekDay = Value
  val Monday = Value(1)
  val Tuesday = Value(2)
  val Wednesday = Value(99)
  val Thursday = Value(4)
  val Friday = Value(5)
  val Saturday = Value(6)
  val Sunday = Value(-3)
}
import WeekDay._

// Scala 3 Enum
enum Month {
  case Jan, Feb, Mar
}

case class Birthday(m: Month, d: WeekDay)

case class TryMe(maybe: scala.util.Try[Boolean])

// Sealed trait w/case classes and objects
sealed trait Vehicle
case class Truck(numberOfWheels: Int) extends Vehicle
case class Car(numberOfWheels: Int, color: String) extends Vehicle
case class Plane(numberOfEngines: Int) extends Vehicle
case class VehicleHolder(v: Vehicle)

sealed trait Flavor
case object Vanilla extends Flavor
case object Chocolate extends Flavor
case object Bourbon extends Flavor
case class FlavorHolder(f: Flavor)

sealed abstract class Animal(val animalType: String) {
  val name: String
}

class Dog(val name: String) extends Animal("Dog")

class Cat(val name: String) extends Animal("Cat")

case class PetOwner(owner: String, pet: Animal)

// Type substitution models
//-------------------------
// 0-level
case class DuoTypes[Q,U](a: Q, b: U)

// 1st level type substitution
case class DuoHolder( a: DuoTypes[Int,Float] )

// 2nd and 3rd level type substitution - option
case class OptHolder( a: Option[DuoTypes[String,Boolean]])
case class OptHolder2( a: Option[Option[DuoTypes[String,Boolean]]])

// 2nd and 3rd level type substitution - either
case class EitherHolder( a: Either[DuoTypes[Int,Float], Option[DuoTypes[String,Boolean]]])

// Alias type substitution
opaque type mystery = DuoTypes[Byte,Short]
case class AliasTypeSub(a: mystery) 

// 1st and 2nd level substitution in class
case class DuoClass( a: DuoTypes[Int,DuoTypes[Byte,Short]] )

// List and Map substitution
case class ListMapSub( a: List[DuoTypes[Int,Byte]], b: Map[String, DuoTypes[Float,Short]])

// Try substitution
case class TryHolder( a: scala.util.Try[DuoTypes[String,Int]] )

// Trait type substitution
trait TypeShell[X] { val x: X }
case class TypeShellHolder(a: TypeShell[Int])

// Union type substitution
case class UnionHolder(a: Int | TypeShell[String])

// Trait type substitution
trait ParamThing[X]{ val id: X }
// case class ParamItem[Y](id:Y) extends ParamThing[Y]

// Intersection type substitution
trait Stackable[T]
trait Floatable[U]

// Non-parameterized Intersection Types
trait InterA
trait InterB
trait InterC
case class IntersectionHolder( a: InterA & InterB & InterC )

// Type member subsitution
trait Body
case class FancyBody(message: String) extends Body

case class Envelope[T <: Body, U](id: String, body: T) {
  type Giraffe = T
  type Foo = Int
}

case class WithScalaArray(
  list: Array[Array[Char]],
  x1: Array[Boolean],
  x2: Array[Byte],
  x3: Array[Char],
  x4: Array[Double],
  x5: Array[Float],
  x6: Array[Int],
  x7: Array[Long],
  x8: Array[Short],
  x9: Array[String]
  )

trait T5[X, Y] { val thing1: X; val thing2: Y }
trait T10[X, Y] { val x: X; val y: Y }
trait T11[W, Z] { val w: W; val z: Z }
case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]
case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B) extends T10[T11[C, T5[D, A]], B]

trait Level1[T,U] { val t: T; val u: Option[List[U]] }
trait Base[A,B] { val a: A; val b: B }
case class L1Class[X,Y]( t: X, u: Option[List[Y]] ) extends Level1[X,Y]
case class BaseClass[X, Y, Z]( a: Level1[X,Z], b: Y ) extends Base[Level1[X,Z],Y]

trait TryIt[X,Y]{ val x: Try[X]; val y: Try[Option[Y]] }
case class TryItC[A,B]( x: Try[A], y: Try[Option[B]]) extends TryIt[A,B]

trait MapIt[X,Y,S,T]{ val x: Map[X,Option[Y]]; val s: Array[S]; val t: Array[List[T]] }
case class MapItC[A,B,W,U]( x: Map[A,Option[B]], s: Array[W], t: Array[List[U]]) extends MapIt[A,B,W,U]

case class CClass[X](x:List[X])
class PClass[Y](val y:List[Y])
case class CClassLevel2[Z](z: Z)
trait ClassistBase[T,U]{ val t: CClass[CClassLevel2[T]]; val u: PClass[U] }
case class ClassistC[A,B](t: CClass[CClassLevel2[A]], u: PClass[B]) extends ClassistBase[A,B]

// Inverted
trait ClassistBaseInv[T,U]{ val t: CClass[T]; val u: PClass[U] }
case class ClassistCInv[A,B](t: CClass[A], u: PClass[B]) extends ClassistBaseInv[A,B]

// Non-Case Scala class handling
class FoomNC(val a: Int, val b: String) {
  @FieldAnno(idx=5) var blah: Boolean = false
  @Ignore var hey: Int = 2
  private var cantSee: Boolean = true
  val nope: Float = 1.2

  private var _age = 0
  def age = _age
  @FieldAnno(idx=2) def age_=(g: Int): Unit = _age = g
}

// Inheritance and Annotations
class InheritSimpleBase(
    @DBKey(index = 50)@Change(name = "bogus") val one:String= "blather"
) {
  // Public data member
  @DBKey(index = 1) @Change(name = "foobar") var two: Int = 5
  var three: Boolean = true

  // Private var or val
  val notOne: Int = 2

  @Ignore var dontseeme: Int = 90

  // Scala-style getter/setter
  private var _four: Double = 0.1
  @DBKey(index = 2) def four: Double = _four
  @Change(name = "quatro") def four_=(a: Double): Unit = _four = a

  private var _dontForget: Int = 9
  def dontForget: Int = _dontForget
  def dontForget_=(a: Int): Unit = _dontForget = a

  private var _unused: Double = 0.1
  @Ignore def unused: Double = _unused
  def unused_=(a: Double): Unit = _unused = a
}

class InheritSimpleChild(
    val extra: String,
    @Change(name = "uno") override val one:String)
  extends InheritSimpleBase(one) {
  @DBKey(index = 99) var foo: Int = 39
  @Ignore var bogus: String = ""

  private var _nada: Double = 0.1
  def nada: Double = _nada
  @Ignore def nada_=(a: Double): Unit = _nada = a
}

// Inheritance and parameterized classes
class ParamBase[T](val thing: T) {
  var item: T = null.asInstanceOf[T]

  private var _cosa: T = null.asInstanceOf[T]
  def cosa: T = _cosa
  def cosa_=(a: T): Unit = _cosa = a
}

class ParamChild[T](override val thing: T) extends ParamBase[T](thing)

// Self-referencing
case class Shape(id: Int, parent: Option[Shape])
case class Person2(name: String, age: Int, boss: Person2)
case class Drawer[T]( id: Int, nextInChain: Option[Drawer[T]], thing: T)


// Implicit Method adds.. (extension methods)
import info._
extension (s: ScalaCaseClassInfo)
  def constructWith[T](args: List[Object]): T = s.infoClass.getConstructors.head.newInstance(args:_*).asInstanceOf[T]

extension (s: JavaClassInfo)
  def constructWith[T](args: List[Object]): T = 
    val asBuilt = s.infoClass.getConstructors.head.newInstance().asInstanceOf[T]
    s.fields.map(f => f.asInstanceOf[JavaFieldInfo].valueSetter.invoke(asBuilt, args(f.index)))
    asBuilt

// Java Collections
case class JColl(
  a: java.util.List[Int],
  b: java.util.Optional[java.util.ArrayList[Int]],
  c: java.util.Stack[String],
  d: java.util.Queue[Map[Int,String]],
  e: java.util.Set[Boolean],
  f: java.util.Map[Int, String]
)

// InTermsOf substitution
trait Basis[T] {
  val a: Int
  val b: String
  val c: T
}
case class Thingy[T]( a: Int, b: String, c: T) extends Basis[T]

// Classes inside objects
object BigObject:
  case class LittleThing(a: Int)