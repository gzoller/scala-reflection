package co.blocke.scala_reflection
package models

import co.blocke.reflect.* 


case class Item(desc:String)
case class Person(name:String, age:Int, item:Item, allDone: Boolean)

case class HasDefaults( a: String = "wow", item: Item = Item("none"), c: Int = 5 )
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
    j: Any
)

case class SelfReferencing( a: String, b: SelfReferencing, c: Int, d: Option[SelfReferencing])

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

// Opaque type aliases
opaque type EMP_ID = Int
case class Employee(eId: EMP_ID, age: Int)

// Value classes
case class IdUser(id: Int) extends AnyVal  // value class
case class Employee2(eId: IdUser, age: Int)

// @Skip_Reflection
@Ignore
case class SkipMe(a: Int, b: String)

// Self-referencing
case class Shape(id: Int, parent: Option[Shape])
case class Person2(name: String, age: Int, boss: Person2)
case class Drawer[T]( id: Int, nextInChain: Option[Drawer[T]], thing: T)

//-------------------<< Non-Case Classes ---------------

// Non-Case Scala class handling
class FoomNC(val a: Int, val b: String, @FieldAnno(idx=0) c: Option[FoomNC]) {
  @FieldAnno(idx=5) @DBKey var blah: Boolean = false
  @Ignore var hey: Int = 2
  private var cantSee: Boolean = true
  val nope: Float = 1.2

  private var _age = 0
  def age = _age
  @FieldAnno(idx=2) def age_=(g: Int): Unit = _age = g
}

// Object/field Annotations
@ClassAnno(name="Foom")
case class WithAnnotation(@FieldAnno(idx=5) id: String)

