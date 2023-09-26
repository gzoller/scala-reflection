package co.blocke.scala_reflection 

// Opaque type aliases
object Foo:
  opaque type EMP_ID = Int

import Foo.*
case class Employee(eId: EMP_ID, age: Int, other: Any)