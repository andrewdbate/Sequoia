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

package com.sequoiareasoner.kernel.logging

/** The type of loggers used to log messages from the Sequoia reasoner.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
abstract class Logger {

  /** Log a message about the system configuration.
    *
    * @param msg the string message.
    */
  def config(msg: String): Unit

  /** Log a message providing tracing information.
    *
    * @param msg the string message.
    */
  def trace(msg: String): Unit

  /** Log an informational message.
    *
    * @param msg the string message.
    */
  def info(msg: String): Unit

  /** Log a message indicating a potential problem.
    *
    * @param msg the string message.
    */
  def warn(msg: String): Unit

  /** Log a message indicating a serious failure.
    *
    * @param msg the string message.
    */
  def error(msg: String): Unit

}
