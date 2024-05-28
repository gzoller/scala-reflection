package co.blocke.scala_reflection
package models

import co.blocke.reflect.*
import neotype.*

// Basic Tasty classes
case class Item(desc: String)
case class Person(name: String, age: Int, item: Item, allDone: Boolean)

case class HasDefaults(a: String = "wow", item: Item = Item("none"), c: Int = 5)
case class WithDefault(a: Int, b: String = "wow")

case class Prim(
    a: Boolean,
    b: Byte,
    c: Char,
    d: Double,
    e: Float,
    f: Int,
    g: Long,
    h: Short,
    i: String,
    j: Any,
    k: scala.math.BigDecimal,
    l: scala.math.BigInt,
    m: java.lang.Boolean,
    n: java.lang.Byte,
    o: java.lang.Character,
    p: java.lang.Double,
    q: java.lang.Float,
    r: java.lang.Integer,
    s: java.lang.Long,
    t: java.lang.Short,
    u: java.lang.Number,
    v: java.math.BigDecimal,
    w: java.math.BigInteger
)
case class Time(
    a: java.time.Duration,
    b: java.time.Instant,
    c: java.time.LocalDate,
    d: java.time.LocalDateTime,
    e: java.time.LocalTime,
    f: java.time.MonthDay,
    g: java.time.OffsetDateTime,
    h: java.time.OffsetTime,
    i: java.time.Period,
    j: java.time.Year,
    k: java.time.YearMonth,
    l: java.time.ZonedDateTime,
    m: java.time.ZoneId,
    n: java.time.ZoneOffset
)
case class Net(
    a: java.net.URL,
    b: java.net.URI,
    c: java.util.UUID
)

// Match / dependent types
type Elem[X] = X match {
  case String      => Char
  case Array[t]    => t
  case Iterable[t] => t
}
case class Definitely(id: Elem[List[Int]], stuff: Elem[String])

case class SelfReferencing(a: String, b: SelfReferencing, c: Int, d: Option[SelfReferencing])

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

// Sealed abstract class
sealed abstract class Animal(val animalType: String) {
  val name: String
}
class Dog(val name: String) extends Animal("Dog")
class Cat(val name: String) extends Animal("Cat")
case class PetOwner(owner: String, pet: Animal)

// Opaque type aliases
opaque type EMP_ID = Int
case class Employee(eId: EMP_ID, age: Int)

// Value classes
case class IdUser(id: Int) extends AnyVal // value class
case class Employee2(eId: IdUser, age: Int)

// @Skip_Reflection
@Ignore
case class SkipMe(a: Int, b: String)

// Self-referencing
case class Shape(id: Int, parent: Option[Shape])
case class Person2(name: String, age: Int, boss: Person2)
case class Drawer[T](id: Int, nextInChain: Option[Drawer[T]], thing: T)

//-------------------<< Non-Case Classes ---------------

// Non-Case Scala class handling
class FoomNC(val a: Int, val b: String, @FieldAnno(idx = 0) c: Option[FoomNC]) {
  @FieldAnno(idx = 5) @DBKey var blah: Boolean = false
  @Ignore var hey: Int = 2
  private var cantSee: Boolean = true
  val nope: Float = 1.2

  private var _age = 1
  def age = _age
  @FieldAnno(idx = 2) def age_=(g: Int): Unit = _age = g
}

// Object/field Annotations
@ClassAnno(name = "Foom")
case class WithAnnotation(@FieldAnno(idx = 5) id: String)

// Java Collections
case class JColl(
    a: java.util.List[Int],
    b: java.util.Optional[java.util.ArrayList[Int]],
    c: java.util.Stack[String],
    d: java.util.Queue[Map[Int, String]],
    e: java.util.Set[Boolean],
    f: java.util.Map[Int, String]
)

// Mixin tests
trait SJCapture {
  var captured: java.util.HashMap[String, ?] =
    new java.util.HashMap[String, Any]()
}
class SJCaptureJava extends SJCapture

case class JJ(jFoo: JavaParam[Int])

trait Blah[T]:
  val x: T

case class UberJS[T](
    a: Option[List[Either[Int, Boolean]]],
    b: java.util.HashMap[String, Int | Long],
    c: Option[UberJS[Int]],
    x: T
) extends Blah[T]:
  type S = String

// Test NeoType and differentiation from Any
type NonEmptyString = NonEmptyString.Type
given NonEmptyString: Newtype[String] with
  override inline def validate(input: String): Boolean =
    input.nonEmpty

type NonEmptyList = NonEmptyList.Type
given NonEmptyList: Newtype[List[Int]] with
  override inline def validate(input: List[Int]): Boolean =
    input.nonEmpty

case class NeoPerson(age: NonEmptyList, desc: NonEmptyString, whatever: Any)

class Parent(val phase: Int, var stuff: List[String]):
  private var _hidden: Boolean = false
  def hidden: Boolean = _hidden
  def hidden_=(h: Boolean) = _hidden = h

  private var _nope: Boolean = false // should not generate due to @Ignore
  @Ignore def nope: Boolean = _nope
  def nope_=(h: Boolean) = _nope = h

  var foo: String = "ok"
  @Ignore var noFoo: String = "not ok"
