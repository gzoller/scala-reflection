package co.blocke.scala_reflection 

case class Shape(id: Int, parent: Option[Shape])
case class Drawer[T]( id: Int, nextInChain: Option[Drawer[T]], thing: T)