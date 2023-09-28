package co.blocke.scala_reflection 

class FoomNC(val a: Int, val b: String, c: Float, d: Option[FoomNC]) {
  var blah: Boolean = false
  @Ignore var hey: Int = 2
  private var cantSee: Boolean = true
  val nope: Float = 1.2

  private var _age = 0L
  @Ignore def age = _age
  def age_=(g: Long): Unit = _age = g

  private var _thing = 0
  def thing = _thing
  def thing_=(g: Int): Unit = _age = g
}
