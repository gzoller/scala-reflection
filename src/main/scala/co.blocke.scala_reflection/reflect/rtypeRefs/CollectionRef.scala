package co.blocke.scala_reflection
package reflect
package rtypeRefs

/** Marker trait for all Scala/Java collections */
trait CollectionRef[R] extends AppliedRef:
  self: RTypeRef[R] =>

  val typedName: TypedName = name + "[" + elementRef.typedName + "]"

  val elementRef: RTypeRef[?]
  val selectLimit: Int = 1

  def select(i: Int): RTypeRef[?] =
    if i == 0 then elementRef
    else throw new ReflectException(s"AppliedType select index $i out of range for ${self.name}")
