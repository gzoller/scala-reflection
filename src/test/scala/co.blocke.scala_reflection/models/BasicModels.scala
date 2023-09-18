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
