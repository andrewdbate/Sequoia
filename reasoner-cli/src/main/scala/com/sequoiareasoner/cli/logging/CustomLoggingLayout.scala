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

package com.sequoiareasoner.cli.logging

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZonedDateTime}

import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.core.LayoutBase

/** Custom log formatter for the command line interface.
  *
  * @author Andrew Bate <code@andtrewbate.com>
  */
class CustomLoggingLayout extends LayoutBase[ILoggingEvent] {

  override def doLayout(event: ILoggingEvent): String = {
    val timeStampInSeconds: Long = event.getTimeStamp / 1000
    val zdt: ZonedDateTime = ZonedDateTime.ofInstant(Instant.ofEpochSecond(timeStampInSeconds), ZoneId.systemDefault)
    val builder = new StringBuilder(128)
    builder ++= zdt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    builder ++= " ["
    builder ++= event.getLevel.toString
    builder ++= "] "
    builder ++= event.getFormattedMessage
    builder ++= System.lineSeparator
    builder.result
  }

}
