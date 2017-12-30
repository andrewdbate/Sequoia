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

object Timer {
  val NotStarted = -1
}

/** Class used to keep track how much time is spent for a specific operation. Timers are primarily used to display
  * information about performance. A timer is started at the beginning of a method and is stopped at the end of that
  * method. Note that the the status of unstopped timers is undefined. A timer also stores how many times the timer has
  * been started so average time spent in a function can be computed.
  *
  * When a timer is used in a recursive function it will typically be started multiple times. This class will only
  * measure the time spent in the first call. This is done by counting how many times a timer is started and time spent
  * is computed only when the number of calls to stop() calls equals the number of calls to start().
  *
  * The [[Timers]] class stores a set of timers and provides functions to start and stop timers.
  *
  * @constructor Create a timer with the specified name.
  * @param name the name of this timer
  */
class Timer(val name: String) {

  /** the total time that has elapsed while the timer was running */
  private[this] var totalTime: Long = 0L

  /** last time timer was started */
  private[this] var startTime: Long = 0L

  /** number of times the timer was started and stopped */
  private[this] var count: Long = 0L

  /** If we are timing recursive functions timer may be started multiple times. we only want to measure time spent in
    * the upper most function call so we need to discard other starts time that has elapsed between last start()-stop() period
    */
  private[this] var startCount: Long = 0L

  /** time that has elapsed between last start()-stop() period */
  private[this] var lastTime: Long = 0L

  reset

  /** Start time timer by recording the time this function is called. If timer is running when
    * this function is called time is not recorded and only an internal counter is updated.
    */
  def start {
    if (startCount == 0) startTime = System.currentTimeMillis
    startCount += 1
  }

  /** Stop the timer, increment the count and update the total time spent. If timer has been
    * started multiple times this function will only decrement the internal counter. Time
    * information is updated only when all starts are evened out by stops.
    *
    * @return the total time spent after last start(), -1 if timer is still running
    */
  def stop: Long = {
    require(isStarted)
    startCount -= 1
    if (isStarted) {
      -1
    } else {
      lastTime = System.currentTimeMillis - startTime
      totalTime += lastTime
      startTime = Timer.NotStarted
      count += 1
      lastTime
    }
  }

  /** Reset all the internal counts associated with this timer. After this function call it will be like
    * timer has never been used.
    */
  def reset {
    totalTime = 0
    startTime = Timer.NotStarted
    startCount = 0
    count = 0
  }

  /** Return true if timer has been started with a {@link start} call but not has been
    * stopped with a {@link stop} call.
    *
    * @return true iff the timer has been started but has not been stopped
    */
  def isStarted: Boolean = startCount > 0

  /** Return the time elapsed (in milliseconds) since the last time this timer was started.
    * If the timer is not running now 0 is returned.
    *
    * @return TODO
    */
  def getElapsed: Long = if (isStarted) System.currentTimeMillis - startTime else 0

  /** Return the total time (in milliseconds) spent while this timer was running. If the timer
    * is running when this function is called time elapsed will be discarded. Therefore, it is
    * advised to use this function only with stopped timers.
    *
    * @return TODO
    */
  def getTotal: Long = totalTime

  /** Return the total number of times this timer has been started and stopped. Note that
    * recursive start operations are computed only once so actual number of times
    * {@link start} function is called may be greater than this amount.
    *
    * @return TODO
    */
  def getCount: Long = count

  /** Return the total time spent (in milliseconds) divided by the number of times this timer
    * has been ran. If the timer is still running elapsed time is discarded. Therefore, it is
    * advised to use this function only with stopped timers.
    *
    * @return TODO
    */
  def getAverage: Double = totalTime / (if (count == 0) 1.0 else count)

  /** Return the total time spent between last start()-stop() period.
    *
    * @return the total time spent between last start()-stop() period
    */
  def getLast: Long = lastTime

  override def toString: String =
    if (startCount > 0) s"Timer $name; Avg: $getAverage; Count: $count; Total: $getTotal; Still running: $startCount."
    else s"Timer $name; Avg: $getAverage; Count: $count; Total: $getTotal."

  def format: String = LongDurationFormat.format(getTotal + getElapsed)

}
