package co.blocke.scala_reflection
package rtypes

/** Base information we keep for all class fields, regardless of whether Scala or Java
  */
trait FieldInfo:
  val index: Int
  val name: String
  val fieldType: RType[?]
  val originalSymbol: Option[TypeSymbol]
  val annotations: Map[String, Map[String, String]]
  lazy val defaultValue: Option[Object]

//------------------------------------------------------------

/** Describes reflected information we gleen from a Scala class field
  *
  * @param index
  * @param name
  * @param fieldType
  * @param annotations
  * @param defaultValueAccessorName
  * @param originalSymbol
  * @param isNonValConstructorField
  */
case class ScalaFieldInfo(
    index: Int,
    name: String,
    fieldType: RType[?],
    annotations: Map[String, Map[String, String]],
    defaultValueAccessorName: Option[(String, String)], // (companion class name, method)
    originalSymbol: Option[TypeSymbol],
    isNonValConstructorField: Boolean = false // meaningful for non-case classes
) extends FieldInfo:

  /** Default values of constructor fields, where present. This is a rare case where the clunky Java reflection way of getting
    *  this information is better... we can get the default values and conveniently store them with the RType, vs some separate
    *  macro call otherwise.
    */
  lazy val defaultValue: Option[Object] = defaultValueAccessorName.map { (companionClass, accessor) =>
    val companion = Class.forName(companionClass)
    val cons = companion.getDeclaredConstructors()
    cons(0).setAccessible(true)
    companion.getMethod(accessor).invoke(cons(0).newInstance())
  }

//------------------------------------------------------------

case class NonConstructorFieldInfo(
    index: Int,
    name: String,
    getterLabel: String,
    setterLabel: String,
    getterIsVal: Boolean,
    fieldType: RType[?],
    annotations: Map[String, Map[String, String]],
    originalSymbol: Option[TypeSymbol] = None
) extends FieldInfo:

  // Can't have default values for non-constructor fields b/c we can't instantiate
  // classes w/o knowing all the constructor parameters
  lazy val defaultValue: Option[Object] = None
