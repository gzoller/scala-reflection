package co.blocke.scala_reflection
package models

// Type substitution models
//-------------------------
// 0-level
case class DuoTypes[Q,U](a: U, b: Q)  // Note intentional reversal of order to test proper type symbol mapping!

// case class DuoBogus(a: Int, b: Boolean)

// 1st level type substitution
// case class DuoHolder( a: DuoBogus )
case class DuoHolder( x: DuoTypes[Int,Float] )
