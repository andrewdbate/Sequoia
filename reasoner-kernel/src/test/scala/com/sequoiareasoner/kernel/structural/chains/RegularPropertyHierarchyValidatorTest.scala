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

package com.sequoiareasoner.kernel.structural.chains

import com.sequoiareasoner.kernel.owl.model._
import com.sequoiareasoner.kernel.{CommonNames, OWLAxiomBuilder}
import org.scalatest.FunSuite

/** Tests if role hierarchies are correctly found to be regular or irregular.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class RegularPropertyHierarchyValidatorTest extends FunSuite {
  import CommonNames._
  import OWLAxiomBuilder._

  // TODO: the property hierarchy should be checked for regularity automatically on loading.

  private[this] def assertRegular(axioms: Set[_ <: Axiom], expectedResultIsRegular: Boolean): Unit = {
    val checker = new RegularPropertyHierarchyValidator
    axioms foreach checker.visit
    checker.visitEnd
    def checkRegular = axioms foreach checker.isValid
    if (expectedResultIsRegular) checkRegular
    else intercept[NonRegularPropertyHierarchyException] { checkRegular }
  }

  test("RIA Regularity 0 [from HermiT]") {
    // This is not regular according to the OWL 2 specification due to SubObjectPropertyOfAxiom(R, topObjectProperty),
    // however, we expect the reasoner to ignore the axiom, and then the R box is considered regular.
    val axioms = Set(
      SubObjectPropertyOfAxiom(R, topObjectProperty),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, topObjectProperty, Q), R),
    )
    assertRegular(axioms, true)
  }

  test("RIA Regularity 1 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(R1, R2),
      SubObjectPropertyOfAxiom(R2, R3),
      SubObjectPropertyOfAxiom(R3, R4),
      SubObjectPropertyOfAxiom(R4, R1),
    )
    assertRegular(axioms, true)
  }

  test("RIA Regularity 2 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, Q), P),
      InverseObjectPropertiesAxiom(P, Q),
    )
    assertRegular(axioms, false)
  }

  test("RIA Regularity 3 [from HermiT]") {
    // The following is in agreement with HermiT 1.3.8, but is in disagreement with FaCT++ 1.6.3 and Pellet 2.3.1.
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, ObjectInverseOf(Q)), P),
      InverseObjectPropertiesAxiom(P, Q),
    )
    assertRegular(axioms, false)
  }

  test("RIA Regularity 4 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, Q, P), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), Q),
      SubObjectPropertyOfAxiom(Q, R),
    )
    assertRegular(axioms, false)
  }

  test("RIA Regularity 5 [from HermiT]") {
    // The following is in disagreement with HermiT 1.3.8.
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, Q, P), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), T),
      SubObjectPropertyOfAxiom(T, R),
      SubObjectPropertyOfAxiom(R, T),
    )
    assertRegular(axioms, false)
  }

  test("RIA Regularity 6 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, ObjectInverseOf(P), P), P),
    )
    assertRegular(axioms, false)
  }

  test("RIA Regularity 7 [from HermiT]") {
    val axioms = Set(
      InverseObjectPropertiesAxiom(P, invP),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(T, invP), T),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, T), P),
    )
    assertRegular(axioms, false)
  }

  test("RIA Regularity 8 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R4, R1), R1),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2), R2),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R2, R3), R3),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R3, R4), R4),
      EquivalentObjectPropertiesAxiom(Set(R1, R2)),
      EquivalentObjectPropertiesAxiom(Set(R2, R3)),
      EquivalentObjectPropertiesAxiom(Set(R3, R4)),
      EquivalentObjectPropertiesAxiom(Set(R4, R1)),
    )
    assertRegular(axioms, true)
  }

  test("RIA Regularity 9 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2, R3), R),
      EquivalentObjectPropertiesAxiom(Set(R2, R)),
    )
    assertRegular(axioms, false)
  }

  test("RIA Regularity 10 [new]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectInverseOf(P), P),
    )
    assertRegular(axioms, true)
  }

}
