package co.blocke.scala_reflection
package rtypes

import Clazzes.*

trait TimeRType

case class DurationRType() extends RType[java.time.Duration] with TimeRType:
  val name = DURATION_CLASS
  val typedName: TypedName = name

case class InstantRType() extends RType[java.time.Instant] with TimeRType:
  val name = INSTANT_CLASS
  val typedName: TypedName = name

case class LocalDateRType() extends RType[java.time.LocalDate] with TimeRType:
  val name = LOCALDATE_CLASS
  val typedName: TypedName = name

case class LocalDateTimeRType() extends RType[java.time.LocalDateTime] with TimeRType:
  val name = LOCALDATETIME_CLASS
  val typedName: TypedName = name

case class LocalTimeRType() extends RType[java.time.LocalTime] with TimeRType:
  val name = LOCALTIME_CLASS
  val typedName: TypedName = name

case class MonthDayRType() extends RType[java.time.MonthDay] with TimeRType:
  val name = MONTHDAY_CLASS
  val typedName: TypedName = name

case class OffsetDateTimeRType() extends RType[java.time.OffsetDateTime] with TimeRType:
  val name = OFFSETDATETIME_CLASS
  val typedName: TypedName = name

case class OffsetTimeRType() extends RType[java.time.OffsetTime] with TimeRType:
  val name = OFFSETTIME_CLASS
  val typedName: TypedName = name

case class PeriodRType() extends RType[java.time.Period] with TimeRType:
  val name = PERIOD_CLASS
  val typedName: TypedName = name

case class YearRType() extends RType[java.time.Year] with TimeRType:
  val name = YEAR_CLASS
  val typedName: TypedName = name

case class YearMonthRType() extends RType[java.time.YearMonth] with TimeRType:
  val name = YEARMONTH_CLASS
  val typedName: TypedName = name

case class ZonedDateTimeRType() extends RType[java.time.ZonedDateTime] with TimeRType:
  val name = ZONEDDATETIME_CLASS
  val typedName: TypedName = name

case class ZoneIdRType() extends RType[java.time.ZoneId] with TimeRType:
  val name = ZONEID_CLASS
  val typedName: TypedName = name

case class ZoneOffsetRType() extends RType[java.time.ZoneOffset] with TimeRType:
  val name = ZONEOFFSET_CLASS
  val typedName: TypedName = name

case class JavaObjectRType() extends RType[java.lang.Object] with TimeRType:
  val name = JOBJECT_CLASS
  val typedName: TypedName = name
