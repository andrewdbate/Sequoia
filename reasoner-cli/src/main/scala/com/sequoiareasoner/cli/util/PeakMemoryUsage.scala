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

import java.lang.management.{ManagementFactory, MemoryPoolMXBean, MemoryType}

object PeakMemoryUsage {

  /** Converts a given number of bytes to a human-readable format in either SI or binary units.
    *
    * @param bytes  the number of bytes
    * @param si     `true` indicates that SI units should be used; `false` indicates use binary.
    * @return human-readable String for the number of bytes
    */
  def humanReadableByteCount(bytes: Long, si: Boolean): String = {
    val unit: Int = if (si) 1000 else 1024
    if (bytes < unit) {
      bytes + " B"
    } else {
      val exp: Int = (math.log(bytes) / math.log(unit)).toInt
      val pre: Array[String] =
        if (si)
          Array("kB", "MB", "GB", "TB", "PB", "EB")
        else
          Array("KiB", "MiB", "GiB", "TiB", "PiB", "EiB")
      f"${bytes / math.pow(unit, exp)}%.1f ${pre(exp - 1)}"
    }
  }

  /** Returns a String detailing the peak memory usage of the heap for the program thus far.
    *
    * @param verbose  `true` if the usage of each heap memory pool should be included separately.
    * @param si       `true` if SI units should be used, otherwise `false` for binary units.
    * @return a String detailing the peak memory usage of the program thus far.
    */
  def getPeakMemoryUsageInfo(verbose: Boolean = false, si: Boolean = false): String = {
    val builder = new StringBuilder
    var total: Long = 0L
    ManagementFactory.getMemoryPoolMXBeans forEach {
      bean: MemoryPoolMXBean => if (bean.getType == MemoryType.HEAP) {
        val peakUsed: Long = bean.getPeakUsage.getUsed
        if (verbose) {
          builder.append("Peak used for ")
          builder.append(bean.getName)
          builder.append(": ")
          builder.append(humanReadableByteCount(peakUsed, si))
          builder.append('\n')
        }
        total = total + peakUsed
      }
    }
    builder.append("Total peak heap used: ")
    builder.append(humanReadableByteCount(total, si))
    builder.result
  }

}
