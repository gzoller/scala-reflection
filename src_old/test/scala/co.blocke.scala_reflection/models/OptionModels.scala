package co.blocke.scala_reflection
package models

// Options
case class NormalOption(a: Option[Int], b: String)
case class NestedOption(a: Option[Option[Int]], b: String)
case class OptionOfClass(
    a: Option[Person] = Some(Person("Mike", 34, Item("something"), false)),
    b: Option[Person] = None,
    c: String
)
case class ParamOption[T](a: Option[T])
case class UnionHavingOption(a: Boolean | Option[Int], b: Boolean | java.util.Optional[Int])
case class OptionHavingUnion(a: Option[Boolean | String])
