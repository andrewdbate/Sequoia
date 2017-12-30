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

package com.sequoiareasoner.cli

/** The type of exceptions thrown from the command-line classes.
  * These exceptions are caught by the main method and printed to the standard error output.
  */
sealed trait CLIException extends RuntimeException

/** An exception containing a message to be reported by the top-level exception handler of the CLI.
  *
  * @param msg the message to report at the top level
  */
case class ExceptionMessage(msg: String) extends RuntimeException(msg) with CLIException

/** A wrapper for an error or exception that is to be reported by the top-level exception handler of the CLI.
  *
  * @param cause the exception to be passed to the top level
  */
case class ExceptionChained(cause: Throwable) extends RuntimeException(cause) with CLIException
