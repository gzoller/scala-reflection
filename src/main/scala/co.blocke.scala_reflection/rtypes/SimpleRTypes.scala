package co.blocke.scala_reflection
package rtypes

import Clazzes.*

trait SimpleRType

case class DurationRType() extends RType[java.time.Duration] with SimpleRType:
  val name = DURATION_CLASS
  val typedName: TypedName = name

case class InstantRType() extends RType[java.time.Instant] with SimpleRType:
  val name = INSTANT_CLASS
  val typedName: TypedName = name

case class LocalDateRType() extends RType[java.time.LocalDate] with SimpleRType:
  val name = LOCALDATE_CLASS
  val typedName: TypedName = name

case class LocalDateTimeRType() extends RType[java.time.LocalDateTime] with SimpleRType:
  val name = LOCALDATETIME_CLASS
  val typedName: TypedName = name

case class LocalTimeRType() extends RType[java.time.LocalTime] with SimpleRType:
  val name = LOCALTIME_CLASS
  val typedName: TypedName = name

case class MonthDayRType() extends RType[java.time.MonthDay] with SimpleRType:
  val name = MONTHDAY_CLASS
  val typedName: TypedName = name

case class OffsetDateTimeRType() extends RType[java.time.OffsetDateTime] with SimpleRType:
  val name = OFFSETDATETIME_CLASS
  val typedName: TypedName = name

case class OffsetTimeRType() extends RType[java.time.OffsetTime] with SimpleRType:
  val name = OFFSETTIME_CLASS
  val typedName: TypedName = name

case class PeriodRType() extends RType[java.time.Period] with SimpleRType:
  val name = PERIOD_CLASS
  val typedName: TypedName = name

case class YearRType() extends RType[java.time.Year] with SimpleRType:
  val name = YEAR_CLASS
  val typedName: TypedName = name

case class YearMonthRType() extends RType[java.time.YearMonth] with SimpleRType:
  val name = YEARMONTH_CLASS
  val typedName: TypedName = name

case class ZonedDateTimeRType() extends RType[java.time.ZonedDateTime] with SimpleRType:
  val name = ZONEDDATETIME_CLASS
  val typedName: TypedName = name

case class ZoneIdRType() extends RType[java.time.ZoneId] with SimpleRType:
  val name = ZONEID_CLASS
  val typedName: TypedName = name

case class ZoneOffsetRType() extends RType[java.time.ZoneOffset] with SimpleRType:
  val name = ZONEOFFSET_CLASS
  val typedName: TypedName = name

case class JavaObjectRType() extends RType[java.lang.Object] with SimpleRType:
  val name = JOBJECT_CLASS
  val typedName: TypedName = name

case class UUIDRType() extends RType[java.util.UUID] with SimpleRType:
  val name = UUID_CLASS
  val typedName: TypedName = name
