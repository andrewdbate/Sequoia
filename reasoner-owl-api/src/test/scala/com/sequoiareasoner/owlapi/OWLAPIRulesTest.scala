/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This file is part of Sequoia, an OWL 2 reasoner that supports the SRIQ subset of OWL 2 DL.
 * Copyright 2018 by Andrew Bate <code@andrewbate.com>.
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

package com.sequoiareasoner.owlapi

import org.scalatest.FunSuite
import org.semanticweb.owlapi.model.OWLRuntimeException

/** Tests to check correct handling of SWRL rules from the OWL API.
  *
  * Sequoia does not currently support SWRL rules, and therefore these tests either check that SWRL rules are
  * safely ignored, or an exception is thrown.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class OWLAPIRulesTest extends FunSuite {
  import OWLAPITestUtils.getComputedAndKnownInput

  private val dir = "rules_test_input"

  test("Check that exception is thrown on a SWRL Rule") {
    assertThrows[SequoiaUnsupportedException] { // The default is to throw an exception.
      val (actual, expected) = getComputedAndKnownInput(dir, "SingleRule")
      assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
    }
  }

}
