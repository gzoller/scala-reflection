package co.blocke.scala_reflection
package reflect
package rtypeRefs

// Common ancestor of ScalaClassRef and TraitRef because both can be sealed

trait Sealable:
  val sealedChildren: List[RTypeRef[?]]
  val childrenAreObject: Boolean
  val uniqueFields: Map[String, List[String]] // Map[FieldNameHash, List[ClassName]]

  def isSealed: Boolean
