package co.blocke.scala_reflection.models

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
