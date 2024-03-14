package co.blocke.scala_reflection
package reflect
package rtypeRefs

/** This file is very much like PrimitiveRef.scala in that holds network types (vs complex types like collections).
  * In theory we could have bundled all this under PrimitiveRef, but technically these types are not language primitives
  * or wrappers around language primitives, so in the name of integrity we'll separate them out here.
  */

import scala.quoted.*
import rtypes.*
import rtypeRefs.*
import Clazzes.*
import util.{JsonField, JsonObjectBuilder}
import scala.math.{BigDecimal, BigInt}

/** Reference for all Java & Scala primitive types
  */

trait NetRef

case class URLRef()(using quotes: Quotes)(using tt: Type[java.net.URL]) extends RTypeRef[java.net.URL] with NetRef:
  import quotes.reflect.*
  val name = Clazzes.URL_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null }.asExprOf[java.net.URL]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[URLRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.net.URL]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "URLRType"),
        JsonField("name", name)
      )
    )

case class URIRef()(using quotes: Quotes)(using tt: Type[java.net.URI]) extends RTypeRef[java.net.URI] with NetRef:
  import quotes.reflect.*
  val name = Clazzes.URI_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null }.asExprOf[java.net.URI]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[URIRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.net.URI]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "URIRType"),
        JsonField("name", name)
      )
    )

// Not really a "net" thing, but... here t'is...
case class UUIDRef()(using quotes: Quotes)(using tt: Type[java.util.UUID]) extends RTypeRef[java.util.UUID] with NetRef:
  import quotes.reflect.*
  val name = Clazzes.UUID_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null }.asExprOf[java.util.UUID]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[UUIDRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.util.UUID]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "UUIDRType"),
        JsonField("name", name)
      )
    )

object NetRef:
  // Pre-bake primitive types w/cached builder functions
  protected[scala_reflection] val simpleTypeMap = Map(
    URL_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => URLRef()(using quotes)(using Type.of[java.net.URL](using quotes)) },
    URI_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => URIRef()(using quotes)(using Type.of[java.net.URI](using quotes)) },
    UUID_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => UUIDRef()(using quotes)(using Type.of[java.util.UUID](using quotes)) }
  )
