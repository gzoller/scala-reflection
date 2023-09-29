package co.blocke.scala_reflection 

class ParamBase[T](val thing: T) {
  var item: Option[T] = None

  private var _cosa: T = null.asInstanceOf[T]
  def cosa: T = _cosa
  def cosa_=(a: T): Unit = _cosa = a
}

// class ParamChild[T](override val thing: T) extends ParamBase[T](thing)

class Foo[T](val thing: T):
  var store: Option[T] = None