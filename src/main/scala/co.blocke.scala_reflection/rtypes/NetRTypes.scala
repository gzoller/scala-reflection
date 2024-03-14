package co.blocke.scala_reflection
package rtypes

import Clazzes.*

trait NetRType

case class URLRType() extends RType[java.net.URL] with NetRType:
  val name = URL_CLASS
  val typedName: TypedName = name

case class URIRType() extends RType[java.net.URI] with NetRType:
  val name = URI_CLASS
  val typedName: TypedName = name

case class UUIDRType() extends RType[java.util.UUID] with NetRType:
  val name = UUID_CLASS
  val typedName: TypedName = name
