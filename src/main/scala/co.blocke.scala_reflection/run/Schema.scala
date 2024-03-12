package co.blocke.scala_reflection
package run

import java.net.URL

/**
  * A *very* sparse implementation of JSON Schema Draft 4.  It is full of holes, but is just
  * enough for what I needed at the time--and had the needed advantage of leveraging scala-reflection
  * to generate the schema from a type.
  * 
  * If there is strong utility for a full-blown JSON Schema utility, that might be something I look
  * at later, unless someone wants to take it up.  I would suggest development of "helper" objects
  * for the more advanced schema operaitons (allOf, anyOf, if/then/else, etc) vs trying to do all that
  * in annotations.
  */

// Reference: https://json-schema.org/UnderstandingJSONSchema.pdf

enum SchemaType:
    case `null`, `boolean`, `object`, `array`, `string`, `number`, `integer`

enum StringFormat:
    case `date-time`, email, hostname, ipv4, ipv6, uuid, uri, url

// opaque type JSON_LITERAL = String

type Schema = StdSchema | EnumSchema

sealed trait StdSchema:
    val `type`: SchemaType
    val description: Option[String]
    val default: Option[String]

// Formats: Dates & Times, Email, Hostnames
case class StringSchema(
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    pattern: Option[String] = None,
    format: Option[StringFormat] = None,
    description: Option[String] = None,
    default: Option[String] = None
    ) extends StdSchema:
    val `type` = SchemaType.`string`

case class IntegerSchema(
    minimum: Option[Long] = None,
    maximum: Option[Long] = None,
    exclusiveMinimum: Option[Long] = None,
    exclusiveMaximum: Option[Long] = None,
    multipleOf: Option[Int] = None,
    description: Option[String] = None,
    default: Option[String] = None
    ) extends StdSchema:
    val `type` = SchemaType.`integer`

case class NumberSchema(
    minimum: Option[Double] = None,
    maximum: Option[Double] = None,
    exclusiveMinimum: Option[Double] = None,
    exclusiveMaximum: Option[Double] = None,
    multipleOf: Option[Int] = None,
    description: Option[String] = None,
    default: Option[String] = None
    ) extends StdSchema:
    val `type` = SchemaType.`number`

case class BooleanSchema(
    description: Option[String] = None,
    default: Option[String] = None
    ) extends StdSchema:
    val `type` = SchemaType.`boolean`

case class NullSchema(description: Option[String] = None) extends StdSchema:
    val `type` = SchemaType.`null`
    val default: Option[String] = None // no default for null possible

case class ArraySchema(
    items: Schema,
    minItems: Option[Int] = None,
    maxItems: Option[Int] = None,
    uniqueItems: Option[Boolean] = None,
    description: Option[String] = None,
    default: Option[String] = None
) extends StdSchema:
    val `type` = SchemaType.`array`

case class TupleSchema(
    prefixItems: List[Schema],
    items: Option[Boolean] = None,
    minItems: Option[Int] = None,
    maxItems: Option[Int] = None,
    uniqueItems: Option[Boolean] = None,
    description: Option[String] = None,
    default: Option[String] = None
) extends StdSchema:
    val `type` = SchemaType.`array`

// Note: patternProperties not implemented at this time (I didn't need them)
case class ObjectSchema(
    properties: Map[String,Schema],
    required: List[String],
    additionalProperties: Boolean = false,
    `$schema`: URL = new URL("http://jsons-schema.org/draft-04/schema#"),
    `$id`: Option[String] = None,
    title: Option[String] = None, 
    description: Option[String] = None,
    default: Option[String] = None
) extends StdSchema:
    val `type` = SchemaType.`object`

// Weird exception to other schemas--no type, or other decorating feature... just the enum values
case class EnumSchema(
    `enum`: List[String]
)

