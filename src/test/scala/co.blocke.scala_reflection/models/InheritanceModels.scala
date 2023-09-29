package co.blocke.scala_reflection
package models

import co.blocke.reflect.* 


// Inheritance and Annotations
class InheritSimpleBase(
    @DBKey(index = 50)@Change(name = "bogus") val one:String= "blather"
) {
  // Public data member
  @DBKey(index = 1) @Change(name = "foobar") var two: Int = 5
  var three: Boolean = true

  // Private var or val
  val notOne: Int = 2

  @Ignore var dontseeme: Int = 90

  // Scala-style getter/setter
  private var _four: Double = 0.1
  @DBKey(index = 2) def four: Double = _four
  @Change(name = "quatro") def four_=(a: Double): Unit = _four = a

  private var _dontForget: Int = 9
  def dontForget: Int = _dontForget
  def dontForget_=(a: Int): Unit = _dontForget = a

  private var _unused: Double = 0.1
  @Ignore def unused: Double = _unused
  def unused_=(a: Double): Unit = _unused = a
}

class InheritSimpleChild(
    val extra: String,
    @Change(name = "uno") override val one:String)
  extends InheritSimpleBase(one) {
  @DBKey(index = 99) var foo: Int = 39
  @Ignore var bogus: String = ""

  private var _nada: Double = 0.1
  def nada: Double = _nada
  @Ignore def nada_=(a: Double): Unit = _nada = a
}

// Inheritance and parameterized classes
class ParamBase[T](val thing: T) {
  var item: T = null.asInstanceOf[T]

  private var _cosa: T = null.asInstanceOf[T]
  def cosa: T = _cosa
  def cosa_=(a: T): Unit = _cosa = a
}

class ParamChild[T](override val thing: T) extends ParamBase[T](thing)