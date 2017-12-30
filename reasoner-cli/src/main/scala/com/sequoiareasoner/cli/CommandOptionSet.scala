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

import scala.collection.mutable

/** A set of [[CommandOption]] objects representing the options for a particular command.
  * Option objects can be added and enumerated.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
final class CommandOptionSet {
  private[this] val longOptions = new mutable.AnyRefMap[String, CommandOption]
  private[this] val shortOptions = new mutable.AnyRefMap[String, CommandOption]
  private[this] val mandatory = new mutable.HashSet[CommandOption]

  /** Add an option to the set. Both the short name and the long name of the option must be distinct from all
    * existing (short and long) option names.
    *
    * @param option  the option to add
    */
  def +=(option: CommandOption): Unit = {
    val shortOption: String = option.shortOption
    val longOption: String = option.longOption
    if (shortOptions.contains(shortOption) || longOptions.contains(shortOption))
      throw ExceptionMessage(s"Duplicate short option for command: $shortOption")
    if (shortOptions.contains(longOption) || longOptions.contains(longOption))
      throw ExceptionMessage(s"Duplicate long option for command: $longOption")
    shortOptions.put(shortOption, option)
    longOptions.put(longOption, option)
    if (option.isMandatory) mandatory += option
  }

  /** Returns `true` if an option exists with the supplied name as its short or long name, and `false` otherwise.
    *
    * @param name  the short or long name to check
    * @return `true` if an option exists with the supplied name as its short or long name, and `false` otherwise.
    */
  def containsOption(name: String): Boolean =
    (shortOptions contains name) || (longOptions contains name)

  /** Return the option with the short or long name matching the supplied name, or `null` if no such option exists.
    *
    * @param name  the short or long name to match
    * @return the matching option, or `null` if no such option exists.
    */
  def getOption(name: String): CommandOption =
    if (shortOptions contains name)
      shortOptions(name)
    else if (longOptions contains name)
      longOptions(name)
    else null

  /**
    * @return all options that must always be supplied by the user for this command.
    */
  def mandatoryOptions: Set[CommandOption] = mandatory.toSet

  /**
    * @return all options contained in this set.
    */
  def allOptions: Set[CommandOption] = longOptions.values.toSet
}
