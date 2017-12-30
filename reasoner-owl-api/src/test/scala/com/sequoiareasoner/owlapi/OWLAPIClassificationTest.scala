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

package com.sequoiareasoner.owlapi

import org.scalatest.FunSuite

/** Tests to check that the computed taxonomy is both correct and complete, using *only* OWL API calls.
  *
  * Some test cases are adapted from the ELK reasoner test suite.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class OWLAPIClassificationTest extends FunSuite {
  import OWLAPITestUtils.getComputedAndKnownInput

  private val dir = "classification_test_input"

  test("Load and classify Ancestors.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "Ancestors")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  // Sequoia does not support ABox reasoning.
  ignore("Load and classify AssertionDisjoint.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "AssertionDisjoint")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify Bottom.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "Bottom")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify Conjunctions.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "Conjunctions")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify Disjoint.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "Disjoint")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  // The OWL API does not correctly support repeated classes in DisjointClasses axioms.
  ignore("Load and classify DisjointSelf.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "DisjointSelf")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify endocarditis.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "endocarditis")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify Existentials.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "Existentials")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify forest.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "forest")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify Inconsistent.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "Inconsistent")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify kangaroo.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "kangaroo")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify PropertyChains.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "PropertyChains")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  ignore("Load and classify PropertyChainsWithInverses.owl using OWL API") { // FIXME: throws an exception.
    val (actual, expected) = getComputedAndKnownInput(dir, "PropertyChainsWithInverses")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify ReflexiveRole.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "ReflexiveRole")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

  test("Load and classify Top.owl using OWL API") {
    val (actual, expected) = getComputedAndKnownInput(dir, "Top")
    assert(actual === expected, "Computed taxonomy differs from known taxonomy.")
  }

}
