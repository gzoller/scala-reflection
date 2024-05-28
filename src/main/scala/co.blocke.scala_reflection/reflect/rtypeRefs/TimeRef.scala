package co.blocke.scala_reflection
package reflect
package rtypeRefs

/** This file is very much like PrimitiveRef.scala in that holds time types.
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

trait TimeRef

case class DurationRef()(using quotes: Quotes)(using tt: Type[java.time.Duration]) extends RTypeRef[java.time.Duration] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.DURATION_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.Duration] }.asExprOf[java.time.Duration]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[DurationRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.Duration]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "DurationRType"),
        JsonField("name", name)
      )
    )

case class InstantRef()(using quotes: Quotes)(using tt: Type[java.time.Instant]) extends RTypeRef[java.time.Instant] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.INSTANT_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.Instant] }.asExprOf[java.time.Instant]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[InstantRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.Instant]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "InstantRType"),
        JsonField("name", name)
      )
    )

case class LocalDateRef()(using quotes: Quotes)(using tt: Type[java.time.LocalDate]) extends RTypeRef[java.time.LocalDate] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.LOCALDATE_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.LocalDate] }.asExprOf[java.time.LocalDate]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[LocalDateRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.LocalDate]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "LocalDateRType"),
        JsonField("name", name)
      )
    )

case class LocalDateTimeRef()(using quotes: Quotes)(using tt: Type[java.time.LocalDateTime]) extends RTypeRef[java.time.LocalDateTime] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.LOCALDATETIME_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.LocalDateTime] }.asExprOf[java.time.LocalDateTime]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[LocalDateTimeRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.LocalDateTime]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "LocalDateTimeRType"),
        JsonField("name", name)
      )
    )

case class LocalTimeRef()(using quotes: Quotes)(using tt: Type[java.time.LocalTime]) extends RTypeRef[java.time.LocalTime] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.LOCALTIME_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.LocalTime] }.asExprOf[java.time.LocalTime]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[LocalTimeRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.LocalTime]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "LocalTimeRType"),
        JsonField("name", name)
      )
    )

case class MonthDayRef()(using quotes: Quotes)(using tt: Type[java.time.MonthDay]) extends RTypeRef[java.time.MonthDay] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.MONTHDAY_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.MonthDay] }.asExprOf[java.time.MonthDay]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[MonthDayRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.MonthDay]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "MonthDayRType"),
        JsonField("name", name)
      )
    )

case class OffsetDateTimeRef()(using quotes: Quotes)(using tt: Type[java.time.OffsetDateTime]) extends RTypeRef[java.time.OffsetDateTime] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.OFFSETDATETIME_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.OffsetDateTime] }.asExprOf[java.time.OffsetDateTime]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[OffsetDateTimeRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.OffsetDateTime]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "OffsetDateTimeRType"),
        JsonField("name", name)
      )
    )

case class OffsetTimeRef()(using quotes: Quotes)(using tt: Type[java.time.OffsetTime]) extends RTypeRef[java.time.OffsetTime] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.OFFSETTIME_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.OffsetTime] }.asExprOf[java.time.OffsetTime]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[OffsetTimeRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.OffsetTime]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "OffsetTimeRType"),
        JsonField("name", name)
      )
    )

case class PeriodRef()(using quotes: Quotes)(using tt: Type[java.time.Period]) extends RTypeRef[java.time.Period] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.PERIOD_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.Period] }.asExprOf[java.time.Period]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[PeriodRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.Period]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "PeriodRType"),
        JsonField("name", name)
      )
    )

case class YearRef()(using quotes: Quotes)(using tt: Type[java.time.Year]) extends RTypeRef[java.time.Year] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.YEAR_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.Year] }.asExprOf[java.time.Year]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[YearRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.Year]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "YearRType"),
        JsonField("name", name)
      )
    )

case class YearMonthRef()(using quotes: Quotes)(using tt: Type[java.time.YearMonth]) extends RTypeRef[java.time.YearMonth] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.YEARMONTH_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.YearMonth] }.asExprOf[java.time.YearMonth]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[YearMonthRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.YearMonth]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "YearMonthRType"),
        JsonField("name", name)
      )
    )

case class ZonedDateTimeRef()(using quotes: Quotes)(using tt: Type[java.time.ZonedDateTime]) extends RTypeRef[java.time.ZonedDateTime] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.ZONEDDATETIME_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.ZonedDateTime] }.asExprOf[java.time.ZonedDateTime]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[ZonedDateTimeRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.ZonedDateTime]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ZonedDateTimeRType"),
        JsonField("name", name)
      )
    )

case class ZoneIdRef()(using quotes: Quotes)(using tt: Type[java.time.ZoneId]) extends RTypeRef[java.time.ZoneId] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.ZONEID_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.ZoneId] }.asExprOf[java.time.ZoneId]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[ZoneIdRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.ZoneId]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ZoneIdRType"),
        JsonField("name", name)
      )
    )

case class ZoneOffsetRef()(using quotes: Quotes)(using tt: Type[java.time.ZoneOffset]) extends RTypeRef[java.time.ZoneOffset] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.ZONEOFFSET_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.time.ZoneOffset] }.asExprOf[java.time.ZoneOffset]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[ZoneOffsetRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.time.ZoneOffset]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "ZoneOffsetRType"),
        JsonField("name", name)
      )
    )

case class JObjectRef()(using quotes: Quotes)(using tt: Type[java.lang.Object]) extends RTypeRef[java.lang.Object] with TimeRef:
  import quotes.reflect.*
  val name = Clazzes.JOBJECT_CLASS
  val typedName: TypedName = name
  override val isNullable = true
  val refType = tt

  val unitVal = '{ null.asInstanceOf[java.lang.Object] }.asExprOf[java.lang.Object]

  val expr =
    Apply(
      Select.unique(New(TypeTree.of[JavaObjectRType]), "<init>"),
      Nil
    ).asExprOf[RType[java.lang.Object]]

  def asJson(sb: StringBuilder)(using quotes: Quotes): Unit =
    JsonObjectBuilder(quotes)(
      sb,
      List(
        JsonField("rtype", "JavaObjectRType"),
        JsonField("name", name)
      )
    )

object TimeRef:
  // Pre-bake primitive types w/cached builder functions
  protected[scala_reflection] val simpleTypeMap = Map(
    DURATION_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => DurationRef()(using quotes)(using Type.of[java.time.Duration](using quotes)) },
    INSTANT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => InstantRef()(using quotes)(using Type.of[java.time.Instant](using quotes)) },
    LOCALDATE_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => LocalDateRef()(using quotes)(using Type.of[java.time.LocalDate](using quotes)) },
    LOCALDATETIME_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => LocalDateTimeRef()(using quotes)(using Type.of[java.time.LocalDateTime](using quotes)) },
    LOCALTIME_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => LocalTimeRef()(using quotes)(using Type.of[java.time.LocalTime](using quotes)) },
    MONTHDAY_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => MonthDayRef()(using quotes)(using Type.of[java.time.MonthDay](using quotes)) },
    OFFSETDATETIME_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => OffsetDateTimeRef()(using quotes)(using Type.of[java.time.OffsetDateTime](using quotes)) },
    OFFSETTIME_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => OffsetTimeRef()(using quotes)(using Type.of[java.time.OffsetTime](using quotes)) },
    PERIOD_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => PeriodRef()(using quotes)(using Type.of[java.time.Period](using quotes)) },
    YEAR_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => YearRef()(using quotes)(using Type.of[java.time.Year](using quotes)) },
    YEARMONTH_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => YearMonthRef()(using quotes)(using Type.of[java.time.YearMonth](using quotes)) },
    ZONEDDATETIME_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => ZonedDateTimeRef()(using quotes)(using Type.of[java.time.ZonedDateTime](using quotes)) },
    ZONEID_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => ZoneIdRef()(using quotes)(using Type.of[java.time.ZoneId](using quotes)) },
    ZONEOFFSET_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => ZoneOffsetRef()(using quotes)(using Type.of[java.time.ZoneOffset](using quotes)) },
    JOBJECT_CLASS.asInstanceOf[TypedName] -> { (quotes: Quotes) => JObjectRef()(using quotes)(using Type.of[java.lang.Object](using quotes)) }
  )
