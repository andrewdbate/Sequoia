/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This file is part of Sequoia, an OWL 2 reasoner that supports the SRIQ subset of OWL 2 DL.
 * Copyright 2017 by Andrew Bate <code@andrewbate.com>.
 *
 * This code is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 only,
 * as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License version 3 for more details (a copy is
 * included in the LICENSE file that accompanied this code).
 *
 * You should have received a copy of the GNU General Public License
 * version 3 along with this work.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.sequoiareasoner.protege.util

/** Utility methods convert to and from String representations of time-based durations.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object TimeUtils {

  /** Number of milliseconds per second. */
  private final val MillisecondsPerSecond = 1000
  /** Number of seconds per minute. */
  private final val SecondsPerMinute = 60
  /** Number of minutes per hour. */
  private final val MinutesPerHour = 60
  /** Number of hours per day. */
  private final val HoursPerDay = 24
  /** Number of days per week. */
  private final val DaysPerWeek = 7
  /** Number of milliseconds per minute. */
  private final val MillisecondsPerMinute = MillisecondsPerSecond * SecondsPerMinute
  /** Number of milliseconds per hour. */
  private final val MillisecondsPerHour = MillisecondsPerMinute * MinutesPerHour
  /** Number of milliseconds per day. */
  private final val MillisecondsPerDay = MillisecondsPerHour * HoursPerDay
  /** Number of milliseconds per week. */
  private final val MillisecondsPerWeek = MillisecondsPerDay * DaysPerWeek

  private def timeUnitStringToMillis(unit: String): Either[String, Int] = unit match {
    case "weeks" | "week" | "w" => Right(MillisecondsPerWeek)
    case "days"  | "day"  | "d" => Right(MillisecondsPerDay)
    case "hours" | "hour" | "h" => Right(MillisecondsPerHour)
    case "minutes" | "minute" | "mins"  | "min"  | "m" => Right(MillisecondsPerMinute)
    case "seconds" | "second" | "secs"  | "sec"  | "s" => Right(MillisecondsPerSecond)
    case "" => Left(s"Time unit unspecified. Append a unit such as 'seconds' or 'minutes'.")
    case _ => Left(s"Unrecognised time unit '$unit'. Try with 'minutes'.")
  }

  private def processAccumulated(accumulatedDigits: StringBuilder, accumulatedLetters: StringBuilder): Either[String, Long] = {
    val duration: Long = accumulatedDigits.result.toLong
    timeUnitStringToMillis(accumulatedLetters.result) map { interval => interval * duration }
  }

  /** Parses a String representation of a time-based duration (such as "1 minute 3 seconds"), and returns the duration
    * in milliseconds if the String is well-formed, otherwise returns an error message from the parser.
    *
    * @param input  the input String to parse
    * @return either an error message if a parsing error was encountered, or the total time in milliseconds.
    */
  def parseTimeOutStringToMillis(input: String): Either[String, Long] = {
    val charArray: Array[Char] = input.toCharArray
    var totalMillis: Long = 0L
    val accumulatedDigits = new StringBuilder
    val accumulatedLetters = new StringBuilder
    var seenWhitespace = false
    var lastSeenWasDigit = false
    for (c <- charArray) c match {
      case c if c.isWhitespace =>
        // Ignore this character.
        seenWhitespace = true
      case c if c.isDigit =>
        if (lastSeenWasDigit) {
          if (seenWhitespace) return Left(s"Whitespace not allowed in numbers.")
          else accumulatedDigits += c // Add to current block of digits.
        } else {
          if (accumulatedDigits.nonEmpty)
            processAccumulated(accumulatedDigits, accumulatedLetters) match {
              case parserError: Left[String, Long] => return parserError
              case Right(part) => totalMillis += part
            }
          accumulatedDigits.clear
          accumulatedLetters.clear
          accumulatedDigits += c // Start of a new block of digits.
        }
        seenWhitespace = false
        lastSeenWasDigit = true
      case c if c.isLetter =>
        if (accumulatedDigits.isEmpty) {
          return Left("Invalid timeout. Try with '1 minute'.")
        } else if (!lastSeenWasDigit) {
          if (seenWhitespace) return Left(s"Whitespace not allowed in time units.")
          else accumulatedLetters += c // Add to current block of letters.
        } else {
          assert(accumulatedLetters.isEmpty)
          accumulatedLetters += c // Start of a new block of letters.
        }
        seenWhitespace = false
        lastSeenWasDigit = false
      case c => return Left(s"Unrecognised character $c.")
    }
    if (accumulatedDigits.isEmpty) {
      Left("Timeout cannot be blank. Try with '1 minute'.")
    } else {
      processAccumulated(accumulatedDigits, accumulatedLetters) match {
        case parserError: Left[String, Long] => return parserError
        case Right(part) => totalMillis += part
      }
      Right(totalMillis)
    }
  }

  /** Returns a String representation of a duration given in milliseconds.
    *
    * @param milliseconds  the duration in millisecond to convert to a String.
    * @return String representation of the specified duration.
    */
  def millisToTimeOutString(milliseconds: Long): String = {
    val days: Long = milliseconds / MillisecondsPerDay
    val hoursPart: Int = ((milliseconds / MillisecondsPerHour) % HoursPerDay).toInt
    val minutesPart: Int = ((milliseconds / MillisecondsPerMinute) % MinutesPerHour).toInt
    val secondsPart: Int = ((milliseconds / MillisecondsPerSecond) % SecondsPerMinute).toInt
    val builder = new StringBuilder
    var isFirst = true
    def append(duration: Long, unit: String) = {
      if (!isFirst) builder.append(' ')
      builder.append(duration).append(' ').append(unit)
      if (duration != 1) builder.append('s')
      isFirst = false
    }
    if (days != 0) append(days, "day")
    if (hoursPart != 0) append(hoursPart, "hour")
    if (minutesPart != 0) append(minutesPart, "minute")
    if (secondsPart != 0) append(secondsPart, "second")
    builder.result
  }

}
