package co.blocke.scala_reflection 

case class Blah[Z](xyz: Option[Z])

class Foo[T,U](val thing: T):
  var store: Option[T] = None
  var abc: U = null.asInstanceOf[U]

// Inheritance and parameterized classes
// class ParamBase[X](val thing: X) {
//   var item: X = null.asInstanceOf[X]
//   var wow: Int = 5

//   private var _cosa: X = null.asInstanceOf[X]
//   def cosa: X = _cosa
//   def cosa_=(a: X): Unit = _cosa = a
// }

// class ParamChild[T]( override val thing: T ) extends ParamBase[T](thing):
//   var abc: T = null.asInstanceOf[T]

class Nada[U]() {
  var foo: Int = 5
  var bar: U = null.asInstanceOf[U]

  private var _cosa: U = null.asInstanceOf[U]
  def cosa: U = _cosa
  def cosa_=(a: U): Unit = _cosa = a
}

class ParamChild[T]( val thing: T ) extends Nada[T]: 
  var abc: T = null.asInstanceOf[T]
  var blah: String = "boom"
  var hey: List[Option[T]] = Nil

// class ParamChild[T]( val thing: T ):
//   var abc: String = "boom"
