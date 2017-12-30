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

package com.sequoiareasoner.cli.util

import org.scalatest.FunSuite

/** Tests for the correct formatting of the peak memory usage.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class PeakMemoryUsageTest extends FunSuite {

  test("Human readable byte count test") {
    /* Test data for the humanReadableByteCount function.
     *                             SI     BINARY
     *                   0:       0 B        0 B
     *                  27:      27 B       27 B
     *                 999:     999 B      999 B
     *                1000:    1.0 kB     1000 B
     *                1023:    1.0 kB     1023 B
     *                1024:    1.0 kB    1.0 KiB
     *                1728:    1.7 kB    1.7 KiB
     *              110592:  110.6 kB  108.0 KiB
     *             7077888:    7.1 MB    6.8 MiB
     *           452984832:  453.0 MB  432.0 MiB
     *         28991029248:   29.0 GB   27.0 GiB
     *       1855425871872:    1.9 TB    1.7 TiB
     * 9223372036854775807:    9.2 EB    8.0 EiB  (Long.MaxValue)
     */
    import PeakMemoryUsage._
    // SI Units
    assert("0 B" === humanReadableByteCount(0, si = true))
    assert("27 B" === humanReadableByteCount(27, si = true))
    assert("999 B" === humanReadableByteCount(999, si = true))
    assert("1.0 kB" === humanReadableByteCount(1000, si = true))
    assert("1.0 kB" === humanReadableByteCount(1023, si = true))
    assert("1.0 kB" === humanReadableByteCount(1024, si = true))
    assert("1.7 kB" === humanReadableByteCount(1728, si = true))
    assert("110.6 kB" === humanReadableByteCount(110592, si = true))
    assert("7.1 MB" === humanReadableByteCount(7077888, si = true))
    assert("453.0 MB" === humanReadableByteCount(452984832, si = true))
    assert("29.0 GB" === humanReadableByteCount(28991029248L, si = true))
    assert("1.9 TB" === humanReadableByteCount(1855425871872L, si = true))
    assert("9.2 EB" === humanReadableByteCount(Long.MaxValue, si = true))
    // Binary Units
    assert("0 B" === humanReadableByteCount(0, si = false))
    assert("27 B" === humanReadableByteCount(27, si = false))
    assert("999 B" === humanReadableByteCount(999, si = false))
    assert("1000 B" === humanReadableByteCount(1000, si = false))
    assert("1023 B" === humanReadableByteCount(1023, si = false))
    assert("1.0 KiB" === humanReadableByteCount(1024, si = false))
    assert("1.7 KiB" === humanReadableByteCount(1728, si = false))
    assert("108.0 KiB" === humanReadableByteCount(110592, si = false))
    assert("6.8 MiB" === humanReadableByteCount(7077888, si = false))
    assert("432.0 MiB" === humanReadableByteCount(452984832L, si = false))
    assert("27.0 GiB" === humanReadableByteCount(28991029248L, si = false))
    assert("1.7 TiB" === humanReadableByteCount(1855425871872L, si = false))
    assert("8.0 EiB" === humanReadableByteCount(Long.MaxValue, si = false))
  }

}
