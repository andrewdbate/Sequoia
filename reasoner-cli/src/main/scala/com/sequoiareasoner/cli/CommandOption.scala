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

package com.sequoiareasoner.cli

/** Represents a CLI command option, i.e. the option name, the long option name and the option argument value given on
  * the command line.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param longOption    the long name of the option
  * @param shortOption   the short name of the option
  * @param description   the description of what this option does
  * @param isMandatory   `true` if this option must always be supplied by the user; `false` otherwise
  * @param defaultValue  the optional default argument value for this option
  */
case class CommandOption(longOption: String, shortOption: String, description: String, isMandatory: Boolean, defaultValue: Option[Any] = None) {
  require(longOption != null)
  require(shortOption != null)
  require(description != null)
  require(defaultValue != null)
  require(longOption.head != '-')
  require(shortOption.head != '-')

  private[this] var value: Any = null
  private[this] var arg: CommandOptionArgCardinality = NoArgument

  def getValue: Any = value
  def getValueAsString: String = {
    if (value != null) return value.toString
    if (defaultValue != None) return defaultValue.get.toString
    return null
  }

  /** Returns the option value as an integer and verifies that the value is a positive integer.
    *
    * @return an integer value
    * @throws CLIException If the option value does not exist or is a not a valid positive integer value.
    */
  @throws[CLIException]
  def getValueAsPositiveInteger: Int = getValueAsInteger(1, Int.MaxValue)

  /** Returns the option value as an integer and verifies that the value is a non-negative integer.
    *
    * @return an integer value
    * @throws CLIException If the option value does not exist or is a not a valid non-negative integer value.
    */
  @throws[CLIException]
  def getValueAsNonNegativeInteger: Int = getValueAsInteger(0, Int.MaxValue)

  /** Returns the option value as an integer.
    *
    * @return an integer value
    * @throws CLIException If the option value does not exist or is a not a valid integer value.
    */
  @throws[CLIException]
  def getValueAsInteger: Int = getValueAsInteger(Int.MinValue, Int.MaxValue)

  /** Returns the option value as an integer and verifies that it is in the given range.
    *
    * @param minAllowed Minimum allowed value for the integer (inclusive)
    * @param maxAllowed Maximum allowed value for the integer (inclusive)
    * @return an integer value in the specified range
    * @throws CLIException If the option value does not exist, is a not a valid integer value, or not in the specified range.
    */
  @throws[CLIException]
  private[this] def getValueAsInteger(minAllowed: Int, maxAllowed: Int): Int = {
    val value: String = getValueAsString
    if (value == null) throw new ExceptionMessage(s"The value for option <$longOption> does not exist.")
    try {
      val intValue: Int = value.toInt
      if (intValue < minAllowed) throw ExceptionMessage(s"The value for option <$longOption> should be greater than or equal to $minAllowed but was: $intValue")
      if (intValue > maxAllowed) throw ExceptionMessage(s"The value for option <$longOption> should be less than or equal to $maxAllowed but was: $intValue")
      return intValue
    } catch {
      case e: NumberFormatException =>
        throw ExceptionMessage(s"The value for option <$longOption> is not a valid integer: $value")
    }
  }

  /** Parses the string argument as a boolean. Returns `true` if the string argument is not `null` and is equal,
    * ignoring case, to the string `"true"`.
    *
    * @return returns the string value as a boolean.
    */
  def getValueAsBoolean: Boolean = {
    val value: String = getValueAsString
    java.lang.Boolean.parseBoolean(value)
  }

  def setValue(value: String): Unit = this.value = value
  def setValue(value: Boolean): Unit = this.value = value

  override def toString: String =
    s"[$longOption, $shortOption, $description, $isMandatory, $value, $defaultValue]"

  def setArg(arg: CommandOptionArgCardinality): Unit = this.arg = arg

  def getArg: CommandOptionArgCardinality = arg
}
