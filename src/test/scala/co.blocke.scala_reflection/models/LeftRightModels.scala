package co.blocke.scala_reflection
package models

// Either
case class BothSides(a: Either[Int,String])
case class EitherWithSelf(a: Person, b: Item, c: Either[Person,Item])
case class BothSidesWithOption(a: scala.util.Either[Int, Option[String]])
// case class BothSidesWithUnion(a: scala.util.Either[Int, String|Boolean])

case class BothSidesParam[Y,Z](a: scala.util.Either[Y, Option[ParamOption[Z]]])
