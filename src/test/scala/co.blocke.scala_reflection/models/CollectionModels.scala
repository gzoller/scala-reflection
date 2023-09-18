package co.blocke.scala_reflection
package models

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

// Tuple
case class TupleTurtle[Z]( t: (Int, Z, List[String], NormalOption))