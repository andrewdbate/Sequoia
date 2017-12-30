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

import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.owl.model._
import com.sequoiareasoner.kernel.{CommonNames, OWLAxiomBuilder}
import org.scalatest.FunSuite

/** Tests if a set of axioms is correct found to adhere or violate the simple properties restriction.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class SimplePropertyRestrictionValidatorTest extends FunSuite {
  import CommonNames._
  import OWLAxiomBuilder._

  private[this] def assertSimple(axioms: Set[_ <: Axiom], expectedResultIsSimple: Boolean): Unit = {
    val checker = new SimplePropertyRestrictionValidator(axioms)
    def checkSimple = axioms foreach checker.isValid
    if (expectedResultIsSimple) checkSimple
    else intercept[UseOfNonSimplePropertyException] { checkSimple }
  }

  test("testSimpleRoles1 [from HermiT]") {
    val axioms = Set(
      TransitiveObjectPropertyAxiom(R),
      SubObjectPropertyOfAxiom(R, P),
      SubClassOfAxiom(C, ObjectMinCardinality(2, P))
    )
    assertSimple(axioms, false)
  }

  test("testSimpleRoles2 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, Q), R),
      SubObjectPropertyOfAxiom(R, P),
      SubClassOfAxiom(C, ObjectMaxCardinality(2, P))
    )
    assertSimple(axioms, false)
  }

  test("testSimpleRoles3 [from HermiT]") {
    val axioms = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, Q), R),
      SubObjectPropertyOfAxiom(R, S),
      InverseObjectPropertiesAxiom(S, invS),
      SubClassOfAxiom(C, ObjectMaxCardinality(2, invS))
    )
    assertSimple(axioms, false)
  }

  test("testSimpleRoles4 [from HermiT]") {
    val axioms = Set(
      TransitiveObjectPropertyAxiom(invR),
      SubObjectPropertyOfAxiom(R, P),
      SubObjectPropertyOfAxiom(P, S),
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(S, invS),
      SubClassOfAxiom(C, ObjectMaxCardinality(2, invS))
    )
    assertSimple(axioms, false)
  }

  test("Invalid Transitivity [from Pellet]") {
    val axioms = Set(
      InverseObjectPropertiesAxiom(invQ, Q),
      InverseObjectPropertiesAxiom(P, invP),
      SubClassOfAxiom(A, ObjectMaxCardinality(1, P)),
      SubClassOfAxiom(B, ObjectMaxCardinality(1, invQ)),
      SubObjectPropertyOfAxiom(T, Q),
      TransitiveObjectPropertyAxiom(invP),
      TransitiveObjectPropertyAxiom(P),
      TransitiveObjectPropertyAxiom(T)
    )
    assertSimple(axioms, false)
  }

  test("Correct Ontology [from Pellet]") {
    val comprises = ObjectProperty(IRI(p, "comprises"))
    val isComprisedOf = ObjectProperty(IRI(p, "isComprisedOf"))
    val hasAgency = ObjectProperty(IRI(p, "hasAgency"))
    val inDepartment = ObjectProperty(IRI(p, "inDepartment"))
    val hasDepartment = ObjectProperty(IRI(p, "hasDepartment"))
    val inBranch = ObjectProperty(IRI(p, "inBranch"))
    val GovernmentUnit = OWLClass(IRI(p, "GovernmentUnit"))
    val Department = OWLClass(IRI(p, "Department"))
    val Branch = OWLClass(IRI(p, "Branch"))
    val Agency = OWLClass(IRI(p, "Agency"))
    val axioms = Set(
      InverseObjectPropertiesAxiom(comprises, isComprisedOf),
      InverseObjectPropertiesAxiom(hasAgency, inDepartment),
      InverseObjectPropertiesAxiom(hasDepartment, inBranch),
      ObjectPropertyDomainAxiom(comprises, GovernmentUnit),
      ObjectPropertyDomainAxiom(hasAgency, Department),
      ObjectPropertyDomainAxiom(hasDepartment, Branch),
      ObjectPropertyDomainAxiom(inBranch, Department),
      ObjectPropertyDomainAxiom(inDepartment, Agency),
      ObjectPropertyDomainAxiom(isComprisedOf, GovernmentUnit),
      ObjectPropertyRangeAxiom(comprises, GovernmentUnit),
      ObjectPropertyRangeAxiom(hasAgency, Agency),
      ObjectPropertyRangeAxiom(hasDepartment, Department),
      ObjectPropertyRangeAxiom(inBranch, Branch),
      ObjectPropertyRangeAxiom(inDepartment, Department),
      ObjectPropertyRangeAxiom(isComprisedOf, GovernmentUnit),
      SubClassOfAxiom(Agency, GovernmentUnit),
      SubClassOfAxiom(Branch, GovernmentUnit),
      SubClassOfAxiom(Department, GovernmentUnit),
      SubObjectPropertyOfAxiom(hasAgency, isComprisedOf),
      SubObjectPropertyOfAxiom(hasDepartment, isComprisedOf),
      SubObjectPropertyOfAxiom(inBranch, comprises),
      SubObjectPropertyOfAxiom(inDepartment, comprises),
      TransitiveObjectPropertyAxiom(comprises),
      TransitiveObjectPropertyAxiom(isComprisedOf)
    )
    assertSimple(axioms, true)
  }

}
