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

/*
 * This file is available under and governed by the GNU General Public
 * License version 3 only, as published by the Free Software Foundation.
 * However, the following notice accompanied the original version of this
 * file:
 *
 * Copyright (c) 2016, Andrew Bate, University of Oxford <andrew.bate@cs.ox.ac.uk>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the copyright holder nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.sequoiareasoner.cli.util

/** A class that provides various formatting options for durations represented in milliseconds.
  * The durations are displayed in terms of hours, minutes, seconds, and milliseconds.
  */
sealed abstract class DurationFormat(formatString: String, hoursVisible: Boolean) {

  /** Format the given duration in milliseconds according to the style defined by this DurationFormat class.
    *
    * @param  durationInMilliseconds duration represented in milliseconds
    * @return duration formatted as a string
    */
  def format(durationInMilliseconds: Long): String = {
    var remaining = durationInMilliseconds
    var hours, minutes, seconds, milliseconds = 0L
    if (hoursVisible) {
      hours = remaining / 3600000
      remaining = remaining - (hours * 3600000)
    } else {
      hours = 0
    }
    minutes = remaining / 60000
    remaining = remaining - (minutes * 60000)
    seconds = remaining / 1000
    milliseconds = remaining - (seconds * 1000)
    formatString format (hours, minutes, seconds, milliseconds)
  }
}

/**
  * Format duration in full format. Example: 0 hour(s) 2 minute(s) 13 second(s) 572 milliseconds(s)
  */
case object FullDurationFormat extends DurationFormat("%d hour(s) %d minute(s) %d second(s) %d milliseconds(s)", true)

/**
  * Format duration in long format. Example: 00:02:13.572
  */
case object LongDurationFormat extends DurationFormat("%02d:%02d:%02d.%03d", true)

/**
  * Format duration in medium format (no milliseconds). Example: 00:02:13
  */
case object MediumDurationFormat extends DurationFormat("%02d:%02d:%02d", true)

/**
  * Format duration in short format (no hours or milliseconds). Example: 00:02
  */
case object ShortDurationFormat extends DurationFormat("%2$02d:%3$02d", false)
