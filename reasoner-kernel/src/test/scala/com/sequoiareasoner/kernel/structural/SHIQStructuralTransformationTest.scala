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

package com.sequoiareasoner.kernel.structural

import com.sequoiareasoner.kernel.{ClauseSetEquivalenceUtilities, CommonNames, OWLAxiomBuilder}
import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.owl.model._
import org.scalatest.{FunSuite, RandomTestOrder}

/** Structural transformation tests of OWL axioms are fall within OWL DL and can be expressed by SHIQ.
  *
  * Each test takes a set of OWL axioms and structurally transforms the axioms into a new set of ontology clauses.
  * The set of clauses produced is compared for equivalence with a known set of correctly transformed clauses,
  * up to variable renaming, function symbol renaming, and auxiliary IRI renaming.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class SHIQStructuralTransformationTest extends FunSuite with RandomTestOrder {
  import ClauseSetEquivalenceUtilities._
  import CommonNames._
  import OWLAxiomBuilder._

  // Passes the current suite implicitly to the decorated sets.
  private[this] implicit val self = this

  // Dummy ontology to allow for the construction of concepts and roles.
  private[this] implicit val dlOntology: DLOntology = null

 // TODO: insufficient test cases where the max cardinalities and exact cardinalities have non-atomic fillers.

  private[this] val AuxSome0 = IRI.some()
  private[this] val AuxSome1 = IRI.some()
  private[this] val AuxSome2 = IRI.some()
  private[this] val AuxSome3 = IRI.some()
  private[this] val AuxAll0 = IRI.all()
  private[this] val AuxAll1 = IRI.all()
  private[this] val AuxAll2 = IRI.all()
  private[this] val AuxAll3 = IRI.all()
  private[this] val AuxDisjunct0 = IRI.disjunct()
  private[this] val AuxDisjunct1 = IRI.disjunct()
  private[this] val AuxDisjunct2 = IRI.disjunct()

  test("Self-Reflexivity 1 structural transformation"){
    val input = Set(
      SubClassOfAxiom(A, ObjectHasSelf(R)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, A), B),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set[OntologyClause](
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, x))),
      OntologyClause(Body(Role(S, z1, x), Concept(A, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(B, x))),
      OntologyClause(Body(Role(R, x, z2)), Head(Role(S, x, z2))),

    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Self-Reflexivity 2 structural transformation"){
    val input = Set(
      SubClassOfAxiom(A, ObjectHasSelf(R)),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      FunctionalObjectPropertyAxiom(R),
    )
    val expected = Set[OntologyClause](
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2)), Head(Equality(z1, z2))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Self-Reflexivity 3 structural transformation"){
    val input = Set(
      SubClassOfAxiom(A, ObjectHasSelf(R)),
      SubClassOfAxiom(ObjectHasSelf(S), B),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set[OntologyClause](
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, x))),
      OntologyClause(Body(Role(S, x, x)), Head(Concept(B, x))),
      OntologyClause(Body(Role(R, x, z1)), Head(Role(S, x, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 1") { // HAND VERIFIED CORRECT!
    val input = DisjointClassesAxiom(C, D)
    val expected = Set(
      OntologyClause(Body(Concept(C, x), Concept(D, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 2") { // HAND VERIFIED CORRECT!
    val input = DisjointClassesAxiom(A, B, C)
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(B, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 3") { // HAND VERIFIED CORRECT! FIXME: need support for topObjectProperty
    val input = EquivalentClassesAxiom(A, ObjectAllValuesFrom(topObjectProperty, ObjectUnionOf(ObjectComplementOf(D), E)))
    val expected = Set(
      OntologyClause(Body(), Head(Concept(A, x), Concept(D, f1))),
      OntologyClause(Body(), Head(Concept(AuxSome0, f1), Concept(A, x))),
      OntologyClause(Body(Concept(AuxSome0, x), Concept(E, x)), Head()),
      OntologyClause(Body(Concept(AuxAll0, x), Concept(D, x)), Head(Concept(E, x))),
      OntologyClause(Body(Role(topObjectProperty, x, z1), Concept(A, x)), Head(Concept(AuxAll0, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 4") { // HAND VERIFIED CORRECT!
    val input = EquivalentClassesAxiom(A, ObjectIntersectionOf(C, D))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x), Concept(D, x)), Head(Concept(A, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 5") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(A, ObjectSomeValuesFrom(R, Thing))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 5a") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(A, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectIntersectionOf(C, D)), ObjectSomeValuesFrom(R, Thing)))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, x, z1), Concept(A, x)), Head(Concept(C, z1))),
      OntologyClause(Body(Role(R, x, z1), Concept(A, x)), Head(Concept(D, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 5b") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectIntersectionOf(C, D)), ObjectSomeValuesFrom(R, Thing)), A)
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Role(AuxSome0, x, f1), Concept(A, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Concept(C, x), Concept(D, x)), Head()),
    )
    val actual = transform(input)
   assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 5c") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(A, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectIntersectionOf(C, D)), ObjectSomeValuesFrom(R, Thing)))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(C, z1))),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(D, z1))),
      OntologyClause(Body(Role(R, x, z1)), Head(Role(AuxSome1, x, f2), Concept(A, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Concept(C, x), Concept(D, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 6") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(A, ObjectSomeValuesFrom(R, C))
    val expected = Set(
      STClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      STClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C, x))),
      STClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      STClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(A, z1)))

    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 7a") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectAllValuesFrom(R, C))))
    val expected = Set(
      OntologyClause(Body(Concept(AuxAll0, x), Role(R, x, z1)), Head(Concept(C, z1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Role(S, x, z2)), Head(Concept(AuxAll0, z2))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 7b") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectAllValuesFrom(R, C))), A)
    val expected = Set(
      OntologyClause(Body(Role(S, x, z1)), Head(Concept(AuxAll0, z1), Concept(A, x))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z11, x)), Head(Role(S, z11, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 7c") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(A, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectAllValuesFrom(R, C))))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Role(S, x, z2)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Concept(AuxAll0, x), Role(R, x, z1)), Head(Concept(C, z1))),
      OntologyClause(Body(Role(S, x, z1)), Head(Concept(AuxAll1, z1), Concept(A, x))),
      OntologyClause(Body(Concept(AuxAll1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(AuxSome2, x, f3))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 8a") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectAllValuesFrom(R, ObjectComplementOf(C)))))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Concept(AuxAll0, x), Role(AuxSome0, z1, x)), Head()),
      OntologyClause(Body(Role(S, z1, x), Concept(AuxAll1, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll1, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 8b") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectAllValuesFrom(R, ObjectComplementOf(C)))), A)
    val expected = Set(
      OntologyClause(Body(Role(S, x, z1)), Head(Concept(AuxAll0, z1), Concept(A, x))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 8c") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(A, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectAllValuesFrom(R, ObjectComplementOf(C)))))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Concept(AuxAll0, x), Role(AuxSome0, z1, x)), Head()),
      OntologyClause(Body(Role(S, z1, x), Concept(AuxAll1, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll1, z1))),
      OntologyClause(Body(Role(S, x, z1)), Head(Concept(AuxAll2, z1), Concept(A, x))),
      OntologyClause(Body(Concept(AuxAll2, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(AuxSome2, x, f3))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 9a") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectUnionOf(ObjectSomeValuesFrom(R, D), ObjectSomeValuesFrom(S, D)))))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Role(S, x, z2)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome1, x, f2), Role(AuxSome2, x, f3))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(S, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 9b") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectUnionOf(ObjectSomeValuesFrom(R, D), ObjectSomeValuesFrom(S, D)))), A)
    val expected = Set(
      OntologyClause(Body(Role(S, x, z1)), Head(Concept(AuxAll0, z1), Concept(A, x))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Concept(AuxAll1, x), Role(AuxSome0, z1, x)), Head()),
      OntologyClause(Body(Concept(AuxAll2, x), Role(AuxSome0, z1, x)), Head()),
      OntologyClause(Body(Role(R, z1, x), Concept(D, x)), Head(Concept(AuxAll1, z1))), // TODO: AuxAll2 could be renamed AuxAll1 to save one clause.
      OntologyClause(Body(Role(S, z1, x), Concept(D, x)), Head(Concept(AuxAll2, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 9c") { // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(A, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(S, ObjectUnionOf(ObjectSomeValuesFrom(R, D), ObjectSomeValuesFrom(S, D)))))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Role(S, x, z2)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome1, x, f2), Role(AuxSome2, x, f3))),

      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(S, x, z1)), Head(Concept(AuxAll1, z1), Concept(A, x))),
      OntologyClause(Body(Concept(AuxAll1, x)), Head(Role(AuxSome3, x, f4))),
      OntologyClause(Body(Role(AuxSome3, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Concept(AuxAll2, x), Role(AuxSome3, z1, x)), Head()),
      OntologyClause(Body(Concept(AuxAll3, x), Role(AuxSome3, z1, x)), Head()),
      OntologyClause(Body(Role(R, z1, x), Concept(D, x)), Head(Concept(AuxAll2, z1))),
      OntologyClause(Body(Role(S, z1, x), Concept(D, x)), Head(Concept(AuxAll3, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 10") { // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(A, ObjectUnionOf(ObjectSomeValuesFrom(R, D), D))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1), Concept(D, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(A, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(D, x)), Head(Concept(A, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 11") { // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(A, ObjectUnionOf(ObjectSomeValuesFrom(R, D), ObjectSomeValuesFrom(S, D)))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1), Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z2, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome1, z2, x)), Head(Role(S, z2, x))),
      OntologyClause(Body(Role(R, z3, x), Concept(D, x)), Head(Concept(A, z3))),
      OntologyClause(Body(Role(S, z4, x), Concept(D, x)), Head(Concept(A, z4))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 12a") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(C, ObjectAllValuesFrom(R, ObjectAllValuesFrom(P, ObjectAllValuesFrom(S, ObjectComplementOf(A)))))
    val expected = Set(
      OntologyClause(Body(Role(S, z1, x), Concept(A, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(P, z1, x), Concept(AuxAll0, x)), Head(Concept(AuxAll1, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll1, x)), Head(Concept(AuxAll2, z1))),
      OntologyClause(Body(Concept(AuxAll2, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 12b") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectAllValuesFrom(R, ObjectAllValuesFrom(P, ObjectAllValuesFrom(S, ObjectComplementOf(A)))), C)
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1), Concept(C, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(P, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(AuxSome2, x, f3))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Concept(A, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(S, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 12c") { // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(C, ObjectAllValuesFrom(R, ObjectAllValuesFrom(P, ObjectAllValuesFrom(S, ObjectComplementOf(A)))))
    val expected = Set(
      OntologyClause(Body(Role(S, z1, x), Concept(A, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(P, z1, x), Concept(AuxAll0, x)), Head(Concept(AuxAll1, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll1, x)), Head(Concept(AuxAll2, z1))),
      OntologyClause(Body(Concept(AuxAll2, x), Concept(C, x)), Head()),
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1), Concept(C, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(P, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(AuxSome2, x, f3))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Concept(A, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(S, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 13") {
    val input = EquivalentClassesAxiom(C, ObjectIntersectionOf(A, ObjectComplementOf(B)))
    val expected = Set(
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x), Concept(C, x))),
      OntologyClause(Body(Concept(C, x), Concept(B, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 14") { // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectAllValuesFrom(R, E))
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1), Concept(X, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Concept(E, x)), Head()),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(E, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 15") {
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(A, B, C))
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(B, x), Concept(C, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 16a") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, C)), ObjectAllValuesFrom(R, ObjectComplementOf(C))))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x), Concept(X, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 16b") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, C)), ObjectAllValuesFrom(R, ObjectComplementOf(C))), X)
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll0, x)), Head(Concept(AuxAll1, z1))),
      OntologyClause(Body(Concept(AuxAll1, x), Concept(C, x)), Head(Role(AuxSome0, x, f1), Concept(X, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 16c") { // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, C)), ObjectAllValuesFrom(R, ObjectComplementOf(C))))
    val expected = Set(
      OntologyClause(Body(Concept(AuxAll0, x), Concept(X, x)), Head()),
      OntologyClause(Body(Concept(AuxAll2, x), Concept(C, x)), Head(Role(AuxSome1, x, f2), Concept(X, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll1, x)), Head(Concept(AuxAll2, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll1, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 17a") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, Thing), ObjectAllValuesFrom(R, ObjectComplementOf(C))))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x), Concept(X, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 17b") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, Thing), ObjectAllValuesFrom(R, ObjectComplementOf(C))), X)
    val expected = Set(
      OntologyClause(Body(Concept(C, x), Role(R, x, z1)), Head(Role(AuxSome0, x, f1), Concept(X, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 17c") { // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, Thing), ObjectAllValuesFrom(R, ObjectComplementOf(C))))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x), Concept(X, x)), Head()),
      OntologyClause(Body(Concept(C, x), Role(R, x, z1)), Head(Role(AuxSome1, x, f2), Concept(X, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 18a") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(P, ObjectSomeValuesFrom(S1, ObjectComplementOf(C)))))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(C, z1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(P, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(S1, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 18b") { // FIXME!! The new output is not very good!
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(P, ObjectSomeValuesFrom(S1, ObjectComplementOf(C)))), X)
    val expected = Set(
      STClause(Body(Concept(AuxAll0, x)), Head(Concept(AuxSome0, f1), Concept(X, x))),
      STClause(Body(Concept(AuxAll0, x)), Head(Role(R, x, f1), Concept(X, x))),
      STClause(Body(Concept(AuxSome0, x), Concept(C, x)), Head()),
      STClause(Body(Role(S1, x, z1), Role(P, z2, x)), Head(Concept(C, z1), Concept(AuxAll0, z2))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 18c") {
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(P, ObjectSomeValuesFrom(S1, ObjectComplementOf(C)))))
    val expected = Set(
      STClause(Body(), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }
/*
  test("Structural transformation 19a") {
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, D), ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(D), E))))
    val expected = Set(
      STClause(Body(Concept(AuxAll0, x), Concept(D, x)), Head(Concept(E, x))),
      STClause(Body(Role(R, x, z1), Concept(X, x)), Head(Concept(AuxAll0, z1))),
      STClause(Body(Role(R, x, z2), Concept(X, x)), Head(Concept(D, z2))),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 19b") {
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, D), ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(D), E))), X)
    val expected = Set(
      STClause(Body(), Head(Concept(AuxSome0, f1),  Concept(AuxSome1, f2),  Concept(X, x))),
      STClause(Body(), Head(Concept(AuxSome0, f1),  Concept(X, x),  Concept(D, f2))),
      STClause(Body(), Head(Concept(AuxSome0, f1),  Role(R, x, f2),  Concept(X, x))),
      STClause(Body(), Head(Role(R, x, f1),  Concept(AuxSome1, f2),  Concept(X, x))),
      STClause(Body(), Head(Role(R, x, f1),  Concept(X, x),  Concept(D, f2))),
      STClause(Body(), Head(Role(R, x, f1),  Role(R, x, f2),  Concept(X, x))),
      STClause(Body(Concept(AuxSome0, x),  Concept(D, x)), Head()),
      STClause(Body(Concept(AuxSome1, x),  Concept(E, x)), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 19c") {
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, D), ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(D), E))))
    val expected = Set(
      STClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 20a") {
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectAllValuesFrom(ObjectInverseOf(R), A)), ObjectSomeValuesFrom(R, Thing)))
    val expected = Set(
      STClause(Body(Concept(X, x)), Head(Role(R, x, f1))),
      STClause(Body(Role(R, x, z1), Concept(X, x)), Head(Concept(AuxAll0, z1))),
      STClause(Body(Role(R, z2, x), Concept(AuxAll0, x)), Head(Concept(A, z2))),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 20b") {
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectAllValuesFrom(ObjectInverseOf(R), A)), ObjectSomeValuesFrom(R, Thing)), X)
    val expected = Set(
      STClause(Body(Concept(AuxSome0, x), Concept(A, x)), Head()),
      STClause(Body(Concept(AuxSome1, x)), Head(Concept(AuxSome0, f2))),
      STClause(Body(Concept(AuxSome1, x)), Head(Role(R, f2, x))),
      STClause(Body(Role(R, x, z3)), Head(Concept(AuxSome1, f3), Concept(X, x))),
      STClause(Body(Role(R, x, z4)), Head(Role(R, x, f3), Concept(X, x))),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 20c") {
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectAllValuesFrom(ObjectInverseOf(R), A)), ObjectSomeValuesFrom(R, Thing)))
    val expected = Set(
      STClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 21a") {
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R1, C), ObjectSomeValuesFrom(R2, ObjectSomeValuesFrom(R1, ObjectComplementOf(C)))))
    val expected = Set(
      STClause(Body(Concept(AuxSome0, x), Concept(C, x)), Head()),
      STClause(Body(Concept(AuxSome1, x)), Head(Concept(AuxSome0, f1))),
      STClause(Body(Concept(AuxSome1, x)), Head(Role(R1, x, f1))),
      STClause(Body(Concept(X, x)), Head(Concept(AuxSome1, f2))),
      STClause(Body(Concept(X, x)), Head(Role(R2, x, f2))),
      STClause(Body(Role(R1, x, z1), Concept(X, x)), Head(Concept(C, z1))),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 21b") {
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R1, C), ObjectSomeValuesFrom(R2, ObjectSomeValuesFrom(R1, ObjectComplementOf(C)))), X)
    val expected = Set(
      STClause(Body(Concept(AuxAll0, x)), Head(Concept(AuxSome2, f3), Concept(X, x))),
      STClause(Body(Concept(AuxAll0, x)), Head(Role(R1, x, f3), Concept(X, x))),
      STClause(Body(Concept(AuxSome2, x), Concept(C, x)), Head()),
      STClause(Body(Role(R1, x, z2), Role(R2, z3, x)), Head(Concept(C, z2), Concept(AuxAll0, z3))),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 21c") {
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R1, C), ObjectSomeValuesFrom(R2, ObjectSomeValuesFrom(R1, ObjectComplementOf(C)))))
    val expected = Set(
      STClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }
*/
  test("Structural transformation 22") {
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectComplementOf(C), A, ObjectComplementOf(B), D))
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(D, x)), Head(Concept(C, x), Concept(B, x), Concept(X, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(X, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(X, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 23") {
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectComplementOf(ObjectIntersectionOf(A, C, D)), ObjectIntersectionOf(B, C, D)))
    val expected = Set(
      OntologyClause(Body(Concept(B, x), Concept(C, x), Concept(D, x)), Head(Concept(AuxDisjunct0, x), Concept(X, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(X, x), Concept(A, x), Concept(C, x), Concept(D, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 24") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectSomeValuesFrom(R, ObjectComplementOf(C)))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Concept(C, x)), Head()),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(C, z1), Concept(X, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 25") {
    val input = EquivalentClassesAxiom(X, ObjectUnionOf(ObjectIntersectionOf(A, B), ObjectIntersectionOf(A, C), ObjectIntersectionOf(B, C)))
    /* TODO: if the aux disjuncts were inlined, then we would get:
     * Concept(A, x) AND Concept(B, x) -> Concept(X, x)
     * Concept(A, x) AND Concept(C, x) -> Concept(X, x)
     * Concept(B, x) AND Concept(C, x) -> Concept(X, x)
     * Concept(X, x) -> Concept(A, x) OR Concept(B, x)
     * Concept(X, x) -> Concept(A, x) OR Concept(C, x) OR Concept(B, x) (*)
     * Concept(X, x) -> Concept(A, x) OR Concept(C, x)
     * Concept(X, x) -> Concept(B, x) OR Concept(A, x) OR Concept(C, x) (*)
     * Concept(X, x) -> Concept(B, x) OR Concept(A, x)
     * Concept(X, x) -> Concept(B, x) OR Concept(C, x)
     * Notice that the two clauses marked (*) are redundant. Could we use this observation to improve the clausification?
     * Since each AuxDisjunct > A and AuxDisjunct > B and AuxDisjunct > C, would it not be better to inline everything
     * immediately in this case?
     */
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(A, x), Concept(C, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(B, x), Concept(C, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(AuxDisjunct0, x), Concept(AuxDisjunct1, x), Concept(AuxDisjunct2, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(AuxDisjunct2, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(AuxDisjunct2, x)), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 26") { // TODO: update and check the expected output
    val input = EquivalentObjectPropertiesAxiom(Set(R, S))
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Role(S, x, z1))),
      OntologyClause(Body(Role(S, x, z2)), Head(Role(R, x, z2))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 27") { // HAND VERIFIED CORRECT! // TODO: update and check the expected output
    val input = InverseObjectPropertiesAxiom(R, S)
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(S, z1, x)), Head(Role(R, x, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 28") {
    val input = SubClassOfAxiom(A, Nothing)
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 29") {
    val input = SubClassOfAxiom(A, ObjectIntersectionOf(D, ObjectComplementOf(C)))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(A, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 30") {
    val input = SubClassOfAxiom(C, A)
    val expected = Set(
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 31") {
    val input = SubClassOfAxiom(C, ObjectAllValuesFrom(R, C))
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Concept(C, x)), Head(Concept(C, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 32") {
    val input = SubClassOfAxiom(C, ObjectAllValuesFrom(R, ObjectIntersectionOf(C3, C11)))
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Concept(C, x)), Head(Concept(C11, z1))),
      OntologyClause(Body(Role(R, x, z2), Concept(C, x)), Head(Concept(C3, z2))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 33") {
    val input = SubClassOfAxiom(C, ObjectIntersectionOf(ObjectUnionOf(ObjectComplementOf(A), B), ObjectUnionOf(ObjectComplementOf(A), ObjectComplementOf(B))))
    val expected = Set(
      OntologyClause(Body(Concept(C, x), Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(C, x), Concept(A, x), Concept(B, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 34") {
    val input = SubClassOfAxiom(C, ObjectIntersectionOf(ObjectUnionOf(ObjectComplementOf(B), A), ObjectUnionOf(A, B)))
    val expected = Set(
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, x), Concept(B, x))),
      OntologyClause(Body(Concept(C, x), Concept(B, x)), Head(Concept(A, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 35") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(C, ObjectSomeValuesFrom(R, C2))
    val expected = Set(
      OntologyClause(Body(Concept(C, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C2, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 36") { // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(C, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(S, D)))
    val expected = Set(
      OntologyClause(Body(Concept(C, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Role(S, x, z2)), Head(Concept(D, z2))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 37") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(C, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C10)))
    val expected = Set(
      OntologyClause(Body(Concept(C, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C10, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C2, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 38") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectAllValuesFrom(R, C), D)
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1), Concept(D, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 39") {
    val input = SubClassOfAxiom(Thing, C)
    val expected = Set(
      OntologyClause(Body(), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }
/*
  ignore("Structural transformation 40") { // TODO: update and check the expected output
    val input = SubClassOfAxiom(Thing, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, ObjectAllValuesFrom(S, ObjectAllValuesFrom(ObjectInverseOf(S), C)))))
    val expected = Set(
      STClause(Body(Concept(AuxAll0, x), Role(R, z1, x)), Head()),
      STClause(Body(Role(R, z2, x), Concept(AuxAll1, x)), Head(Concept(AuxAll0, z2))),
      STClause(Body(Role(S, z3, x), Role(S, z4, x)), Head(Concept(C, z3), Concept(AuxAll1, z4))),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }*/

  ignore("Structural transformation 41") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(Thing, ObjectAllValuesFrom(R, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(ObjectInverseOf(R), ObjectSomeValuesFrom(R, ObjectAllValuesFrom(ObjectInverseOf(R), ObjectAllValuesFrom(R, C)))))))
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Role(R, z2, x)), Head(Concept(AuxAll1, z2))),
      OntologyClause(Body(Concept(AuxAll1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Role(R, z2, x)), Head(Concept(AuxAll2, z2))),
      OntologyClause(Body(Concept(AuxAll2, x), Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 42") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(Thing, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectAllValuesFrom(ObjectInverseOf(R), ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, C))))))
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, f1, x))),
      OntologyClause(Body(Role(AuxSome0, x, z1)), Head(Role(R, x, z1))),
      OntologyClause(Body(Role(AuxSome0, x, z1), Role(R, z2, x)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Concept(AuxAll0, x), Role(R, x, z1)), Head(Concept(AuxAll1, z1))),
      OntologyClause(Body(Concept(AuxAll1, x), Role(R, x, z1)), Head(Concept(AuxAll2, z1))),
      OntologyClause(Body(Concept(AuxAll2, x), Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 43") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(Thing, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectAllValuesFrom(ObjectInverseOf(R), ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, C)))))))
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, f1, x))),
      OntologyClause(Body(Role(AuxSome0, x, z1)), Head(Role(R, x, z1))),
      OntologyClause(Body(Role(AuxSome0, x, z1), Role(R, z2, x)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome1, f2, x))), // FIXME: this clause should not be generated. Add test that Internal -> Internal is never generated.
      OntologyClause(Body(Role(AuxSome1, x, z1)), Head(Role(R, x, z1))),
      OntologyClause(Body(Role(AuxSome1, x, z1)), Head(Role(AuxSome2, f3, x))),
      OntologyClause(Body(Role(AuxSome2, x, z1)), Head(Role(R, x, z1))),
      OntologyClause(Body(Role(AuxSome2, x, z1), Role(R, x, z2)), Head(Concept(AuxAll1, z2))),
      OntologyClause(Body(Concept(AuxAll1, x), Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 44") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(Thing, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectSomeValuesFrom(R, C)))
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, f1, x))),
      OntologyClause(Body(Role(AuxSome0, x, z1)), Head(Role(R, x, z1))),
      OntologyClause(Body(Role(AuxSome0, x, z1)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 45") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(Thing, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectSomeValuesFrom(R, ObjectAllValuesFrom(R, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectComplementOf(C))))))
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, f1, x))),
      OntologyClause(Body(Role(AuxSome0, x, z1)), Head(Role(R, x, z1))),
      OntologyClause(Body(Role(AuxSome0, x, z1)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Role(R, x, z2)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome2, f3, x))), // FIXME: this could should not be generated.
      OntologyClause(Body(Role(AuxSome2, x, z1)), Head(Role(R, x, z1))),
      OntologyClause(Body(Role(AuxSome2, x, z1), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 46") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, C))
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 47") { // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(Thing, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(R, ObjectSomeValuesFrom(R, ObjectComplementOf(C)))))
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(S, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Role(R, x, z2)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 48") {
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(C1, C4, C5))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Concept(C1, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(C4, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(C5, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 49") { // TODO: update and check the expected output
    val input = SubObjectPropertyOfAxiom(P, S1)
    val expected = Set(
      OntologyClause(Body(Role(P, x, z1)), Head(Role(S1, x, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 50a"){ // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(2, son, male), ObjectMinCardinality(2, daughter, ObjectComplementOf(male))))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f4, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome1, x, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Role(AuxSome1, x, f4))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(male, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(son, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(daughter, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Concept(male, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 50b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectMinCardinality(2, son, male), ObjectMinCardinality(2, daughter, ObjectComplementOf(male))), A)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }
/*
  ignore("Structural transformation 50c"){
    val input = EquivalentClassesAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(2, son, male), ObjectMinCardinality(2, daughter, ObjectComplementOf(male))))
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(male, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(male, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(AuxSome0, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(AuxSome0, f4))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f4, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Role(daughter, x, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Role(daughter, x, f4))),
      OntologyClause(Body(Concept(A, x)), Head(Role(son, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(son, x, f2))),
      OntologyClause(Body(Concept(AuxSome0, x), Concept(male, x)), Head()),
      OntologyClause(Body(Concept(AuxAll0, x), Role(daughter, x, z2)), Head(Concept(A, x), Concept(male, z2))),
      OntologyClause(Body(Role(son, z1, x), Concept(male, x)), Head(Concept(AuxAll0, z1))),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }
*/
  ignore("Structural transformation 51a"){ // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(B, ObjectMinCardinality(4, R))
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f4, f3))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f4, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f4, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f4))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 51b"){
    val input = SubClassOfAxiom(ObjectMinCardinality(4, R), B)
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2), Role(R, x, z3), Role(R, x, z4)),
        Head(Equality(z1, z2), Equality(z1, z3), Equality(z1, z4), Equality(z2, z3), Equality(z2, z4), Equality(z3, z4), Concept(B, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 51c"){ // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(B, ObjectMinCardinality(4, R))
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f4, f3))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f4, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f4, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Concept(B, x)), Head(Role(AuxSome0, x, f4))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2), Role(R, x, z3), Role(R, x, z4)),
        Head(Equality(z1, z2), Equality(z1, z3), Equality(z1, z4), Equality(z2, z3), Equality(z2, z4), Equality(z3, z4), Concept(B, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 52a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(A, ObjectExactCardinality(4, R, B)))
    val expected = Set(
      OntologyClause(Body(Concept(B, x), Role(R, z1, x)), Head(Role(AuxAll0, z1, x))),
      OntologyClause(Body(Concept(X, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f4))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f4, f3))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f4, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f4, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Role(AuxAll0, x, z4), Role(AuxAll0, x, z1), Role(AuxAll0, x, z5), Role(AuxAll0, x, z2), Role(AuxAll0, x, z3), Concept(X, x)),
        Head(Equality(z1, z2), Equality(z1, z3), Equality(z1, z4), Equality(z1, z5), Equality(z2, z3), Equality(z2, z4), Equality(z2, z5), Equality(z3, z4), Equality(z3, z5), Equality(z4, z5))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 52b"){ // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectIntersectionOf(A, ObjectExactCardinality(2, R, B)), X)
    val expected = Set(
      OntologyClause(Body(Concept(B, x), Role(R, z1, x)), Head(Role(AuxAll0, z1, x))),
      OntologyClause(Body(Role(AuxAll0, x, z1), Role(AuxAll0, x, z2), Concept(A, x)), Head(Equality(z1, z2), Concept(AuxDisjunct0, x), Concept(X, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(B, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 52c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(A, ObjectExactCardinality(4, R, B)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 53a"){ // HAND VERIFIED CORRECT!
    val input = SubClassOfAxiom(X, ObjectAllValuesFrom(R, ObjectMaxCardinality(1, S)))
    val expected = Set(
      OntologyClause(Body(Concept(AuxAll0, x), Role(S, x, z1), Role(S, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(AuxAll0, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 53b"){ // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectAllValuesFrom(R, ObjectMaxCardinality(1, S)), X)
    val expected = Set(
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1), Concept(X, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f3))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(S, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 53c"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectAllValuesFrom(R, ObjectMaxCardinality(1, S)))
    val expected = Set(
      OntologyClause(Body(Concept(AuxAll0, x), Role(S, x, z1), Role(S, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(), Head(Role(AuxSome0, x, f1), Concept(X, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(AuxSome1, x, f3))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(S, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 54a"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(C2, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R)))))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Concept(C2, x))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, f1, x))),
      OntologyClause(Body(Role(AuxSome0, x, z2)), Head(Role(R, x, z2))),
      OntologyClause(Body(Role(AuxSome0, x, z2)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome0, x, z2), Role(R, x, z1), Role(R, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 54b"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectIntersectionOf(C2, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R)))), X)
    val expected = Set(
      OntologyClause(Body(Concept(C2, x), Role(R, z1, x)), Head(Concept(AuxAll0, z1), Concept(X, x))),
      OntologyClause(Body(Concept(AuxAll1, x), Concept(AuxAll0, x)), Head(Concept(AuxDisjunct0, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(C1, x)), Head(Concept(AuxAll1, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 54c"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(C2, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R)))))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Concept(C2, x))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, f1, x))),
      OntologyClause(Body(Role(AuxSome0, x, z2)), Head(Role(R, x, z2))),
      OntologyClause(Body(Role(AuxSome0, x, z2)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Role(AuxSome0, x, z2), Role(R, x, z1), Role(R, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Concept(C2, x), Role(R, z1, x)), Head(Concept(AuxAll0, z1), Concept(X, x))),
      OntologyClause(Body(Concept(AuxAll1, x), Concept(AuxAll0, x)), Head(Concept(AuxDisjunct0, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f4, f3))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome2, x, f3))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome2, x, f4))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(C1, x)), Head(Concept(AuxAll1, z1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 55a"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R), ObjectMaxCardinality(1, S)))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(A, z1))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1), Role(R, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Concept(X, x), Role(S, x, z1), Role(S, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 55b"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R), ObjectMaxCardinality(1, S)), X)
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2), Role(R, x, z3)), Head(Equality(z1, z2), Equality(z1, z3), Equality(z2, z3), Concept(X, x), Role(AuxSome0, x, f1), Concept(AuxDisjunct0, x), Concept(AuxDisjunct1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x), Concept(A, x)), Head()),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome1, x, f2))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome1, x, f3))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Inequality(f5, f4))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Role(AuxSome2, x, f4))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Role(AuxSome2, x, f5))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(S, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 55c"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R), ObjectMaxCardinality(1, S)))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(A, z1))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1), Role(R, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Concept(X, x), Role(S, x, z1), Role(S, x, z2)), Head(Equality(z1, z2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2), Role(R, x, z3)), Head(Equality(z1, z2), Equality(z1, z3), Equality(z2, z3), Concept(X, x), Role(AuxSome1, x, f4), Concept(AuxDisjunct0, x), Concept(AuxDisjunct1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Concept(A, x)), Head()),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f6, f5))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome2, x, f5))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome2, x, f6))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Inequality(f8, f7))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Role(AuxSome3, x, f7))),
      OntologyClause(Body(Concept(AuxDisjunct1, x)), Head(Role(AuxSome3, x, f8))),
      OntologyClause(Body(Role(AuxSome3, z1, x)), Head(Role(S, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 56a"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C)))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(A, z1))),
      OntologyClause(Body(Role(AuxAll0, x, z1), Role(AuxAll0, x, z2), Concept(X, x)), Head(Equality(z1, z2))),
      OntologyClause(Body(Concept(C, x), Role(R, z1, x)), Head(Role(AuxAll0, z1, x)))

    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 56b"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C)), X)
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2), Role(R, x, z3)), Head(Equality(z1, z2), Equality(z1, z3), Equality(z2, z3), Concept(AuxDisjunct0, x), Role(AuxSome1, x, f1), Concept(X, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x), Concept(A, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 56c"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C)))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f3))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Concept(X, x), Role(R, x, z1)), Head(Concept(A, z1))),
      OntologyClause(Body(Role(AuxAll0, x, z1), Role(AuxAll0, x, z2), Concept(X, x)), Head(Equality(z1, z2))),
      OntologyClause(Body(Concept(C, x), Role(R, z1, x)), Head(Role(AuxAll0, z1, x))),
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2), Role(R, x, z3)), Head(Equality(z1, z2), Equality(z1, z3), Equality(z2, z3), Concept(AuxDisjunct0, x), Role(AuxSome2, x, f4), Concept(X, x))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Inequality(f6, f5))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome1, x, f6))),
      OntologyClause(Body(Concept(AuxDisjunct0, x)), Head(Role(AuxSome1, x, f5))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(AuxSome1, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxSome2, z1, x), Concept(A, x)), Head()),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }
/*
  test("Structural transformation 57a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 57b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 57c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 58a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(ObjectMinCardinality(2, S)), C)), ObjectAllValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 58b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(ObjectMinCardinality(2, S)), C)), ObjectAllValuesFrom(R, D)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 58c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(ObjectMinCardinality(2, S)), C)), ObjectAllValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 59a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectComplementOf(C), ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invS, C)), ObjectSomeValuesFrom(S, C)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 59b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectComplementOf(C), ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invS, C)), ObjectSomeValuesFrom(S, C)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 59c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectComplementOf(C), ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invS, C)), ObjectSomeValuesFrom(S, C)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 60a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 60b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 60c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 61a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 61b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 61c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 62a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectMaxCardinality(2, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 62b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectMaxCardinality(2, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 62c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectMaxCardinality(2, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 63a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectMinCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 63b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectMinCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 63c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectMinCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 64a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectMinCardinality(2, R), ObjectMaxCardinality(1, R)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 64b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectMinCardinality(2, R), ObjectMaxCardinality(1, R)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 64c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectMinCardinality(2, R), ObjectMaxCardinality(1, R)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 65a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, A), ObjectMaxCardinality(1, R, A), ObjectSomeValuesFrom(R, B), ObjectMaxCardinality(1, R, B)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 65b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, A), ObjectMaxCardinality(1, R, A), ObjectSomeValuesFrom(R, B), ObjectMaxCardinality(1, R, B)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 65c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, A), ObjectMaxCardinality(1, R, A), ObjectSomeValuesFrom(R, B), ObjectMaxCardinality(1, R, B)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 66a"){
    val input = SubClassOfAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectSomeValuesFrom(R, C2), ObjectSomeValuesFrom(R, C3), ObjectMaxCardinality(2, R)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 66b"){
    val input = SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectSomeValuesFrom(R, C2), ObjectSomeValuesFrom(R, C3), ObjectMaxCardinality(2, R)), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 66c"){
    val input = EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectSomeValuesFrom(R, C2), ObjectSomeValuesFrom(R, C3), ObjectMaxCardinality(2, R)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }*/

  ignore("Structural transformation 67a"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectMinCardinality(2, R))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 67b"){ // HAND VERIFIED CORRECT!
    val input = SubClassOfAxiom(ObjectMinCardinality(2, R), X)
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2)), Head(Equality(z1, z2), Concept(X, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 67c"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectMinCardinality(2, R))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2)), Head(Equality(z1, z2), Concept(X, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 68a"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = SubClassOfAxiom(X, ObjectMinCardinality(2, R, D))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 68b"){ // HAND VERIFIED CORRECT!
    val input = SubClassOfAxiom(ObjectMinCardinality(2, R, D), X)
    val expected = Set(
      OntologyClause(Body(Role(AuxAll0, x, z1), Role(AuxAll0, x, z2)), Head(Equality(z1, z2), Concept(X, x))),
      OntologyClause(Body(Concept(D, x), Role(R, z1, x)), Head(Role(AuxAll0, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Structural transformation 68c"){ // HAND VERIFIED CORRECT! // FIXME: fails due to clausification changes.
    val input = EquivalentClassesAxiom(X, ObjectMinCardinality(2, R, D))
    val expected = Set(
      OntologyClause(Body(Concept(X, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f1))),
      OntologyClause(Body(Concept(X, x)), Head(Role(AuxSome0, x, f2))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(AuxSome0, z1, x)), Head(Role(R, z1, x))),
      OntologyClause(Body(Role(AuxAll0, x, z1), Role(AuxAll0, x, z2)), Head(Equality(z1, z2), Concept(X, x))),
      OntologyClause(Body(Concept(D, x), Role(R, z1, x)), Head(Role(AuxAll0, z1, x))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }
/*
  test("Structural transformation 69a"){
    val input = SubClassOfAxiom(X, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R, C1))))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 69b"){
    val input = SubClassOfAxiom(ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R, C1))), X)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 69c"){
    val input = EquivalentClassesAxiom(X, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R, C1))))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }


  test("Structural transformation 70"){
    val input = FunctionalObjectPropertyAxiom(F0)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 71"){
    val input = InverseFunctionalObjectPropertyAxiom(R)
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 72"){
    val input = SubClassOfAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(1, R, X), ObjectMaxCardinality(1, R, X)))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 73"){
    val input = SubClassOfAxiom(A, ObjectMaxCardinality(2, R))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 75"){
    val input = SubClassOfAxiom(Thing, ObjectUnionOf(ObjectMinCardinality(2, R, C), ObjectMinCardinality(2, R, D), B))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }

  test("Structural transformation 76"){
    val input = SubClassOfAxiom(C2, ObjectIntersectionOf(X1,
      ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(X2), ObjectAllValuesFrom(S, ObjectUnionOf(Y1, Y2)))),
      ObjectExactCardinality(1, R, ObjectIntersectionOf(X2, ObjectSomeValuesFrom(S, Y1))),
      ObjectExactCardinality(3, R, ObjectIntersectionOf(X2, ObjectSomeValuesFrom(S, Y2)))))
    val expected = Set(
      OntologyClause(Body(), Head()),
    )
    val actual = clausify(input)
    assert(decorate(actual) === decorate(expected))
  }
*/
/*
[BEGIN] Extra test case 1

EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1)
                                               ObjectSomeValuesFrom(R, C2)
                                               ObjectAllValuesFrom(R, D)))
SubClassOfAxiom(D, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, A), ObjectSomeValuesFrom(R1, A), ObjectSomeValuesFrom(R2, A)
                                        ObjectSomeValuesFrom(R3, A), ObjectSomeValuesFrom(R4, A), ObjectSomeValuesFrom(R5, A)
                                        ObjectSomeValuesFrom(R6, A), ObjectSomeValuesFrom(R7, A), ObjectSomeValuesFrom(R8, A)
                                        ObjectSomeValuesFrom(R9, A)))
SubClassOfAxiom(X, ObjectIntersectionOf(ObjectUnionOf(A0, B0), ObjectUnionOf(A1, B1), ObjectUnionOf(A2, B2)
                                        ObjectUnionOf(A3, B3), ObjectUnionOf(A4, B4), ObjectUnionOf(A5, B5)
                                        ObjectUnionOf(A6, B6), ObjectUnionOf(A7, B7), ObjectUnionOf(A8, B8)
                                        ObjectUnionOf(A9, B9), ObjectUnionOf(A10, B10), ObjectUnionOf(A11, B11)
                                        ObjectUnionOf(A12, B12), ObjectUnionOf(A13, B13), ObjectUnionOf(A14, B14)
                                        ObjectUnionOf(A15, B15), ObjectUnionOf(A16, B16), ObjectUnionOf(A17, B17)
                                        ObjectUnionOf(A18, B18), ObjectUnionOf(A19, B19), ObjectUnionOf(A20, B20)
                                        ObjectUnionOf(A21, B21), ObjectUnionOf(A22, B22), ObjectUnionOf(A23, B23)
                                        ObjectUnionOf(A24, B24), ObjectUnionOf(A25, B25), ObjectUnionOf(A26, B26)
                                        ObjectUnionOf(A27, B27), ObjectUnionOf(A28, B28), ObjectUnionOf(A29, B29)
                                        ObjectUnionOf(A30, B30), ObjectUnionOf(A31, B31), ObjectUnionOf(C4, C6), ObjectUnionOf(C5, C7)))
EquivalentClassesAxiom(X, ObjectIntersectionOf(A, ObjectSomeValuesFrom(S, ObjectIntersectionOf(
  ObjectSomeValuesFrom(R, Thing)
  ObjectSomeValuesFrom(P, Thing)
  ObjectAllValuesFrom(R, C)
  ObjectAllValuesFrom(P, ObjectSomeValuesFrom(R, Thing))
  ObjectAllValuesFrom(P, ObjectSomeValuesFrom(P, Thing))
  ObjectAllValuesFrom(P, ObjectAllValuesFrom(R, C))))))
EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R1, Thing)
                                               ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R1, C)))
                                               ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R1, ObjectComplementOf(C))))))
[END} Extra test case 1

[BEGIN] Extra test case 2


      SubClassOfAxiom(C2, ObjectIntersectionOf(Corolla,
        ObjectAllValuesFrom(hasPart, ObjectUnionOf(ObjectComplementOf(Petal), ObjectAllValuesFrom(hasShape, ObjectUnionOf(ps1, ps2)))),
        ObjectExactCardinality(hasPart, 1, Some(ObjectIntersectionOf(Petal, ObjectSomeValuesFrom(hasShape, ps1)))),
        ObjectExactCardinality(hasPart, 3, Some(ObjectIntersectionOf(Petal, ObjectSomeValuesFrom(hasShape, ps2)))))),

      EquivalentClassesAxiom(Complex4a, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R1, ObjectIntersectionOf(ObjectMaxCardinality(tt, 1, None), ObjectSomeValuesFrom(t1, C))),
        ObjectSomeValuesFrom(R2, ObjectIntersectionOf(ObjectMaxCardinality(tt, 1, None), ObjectSomeValuesFrom(t2, D))),
        ObjectSomeValuesFrom(R2, ObjectIntersectionOf(ObjectMaxCardinality(tt, 1, None), ObjectSomeValuesFrom(t2, D))),
        ObjectSomeValuesFrom(R3, ObjectIntersectionOf(ObjectMaxCardinality(tt, 1, None), ObjectSomeValuesFrom(t3, E)))
      )),

      EquivalentClassesAxiom(X1, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectMaxCardinality(R, 3, None))),

      EquivalentClassesAxiom(X2, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectMaxCardinality(R, 3, None))),

      EquivalentClassesAxiom(X3, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectMaxCardinality(R, 3, None))),

      EquivalentClassesAxiom(X4, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C4, C)),
        ObjectMaxCardinality(R, 4, None))),

      EquivalentClassesAxiom(X5, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectSomeValuesFrom(R, C5),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C4, C)),
        ObjectMaxCardinality(R, 4, None))),

      EquivalentClassesAxiom(X6, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectSomeValuesFrom(R, C5),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C4, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C5, C)),
        ObjectMaxCardinality(R, 5, None)))

  EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(S, ObjectIntersectionOf(ObjectComplementOf(C), ObjectComplementOf(D))),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectMaxCardinality(invR, 1, None), ObjectSomeValuesFrom(invR, ObjectAllValuesFrom(S, C)))))
      )

      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, A),
        ObjectMinCardinality(R, 3, Some(C)),
        ObjectMinCardinality(R, 3, Some(D)),
        ObjectMinCardinality(R, 2, Some(ObjectIntersectionOf(E, ObjectComplementOf(ObjectIntersectionOf(C, D))))),
        ObjectMaxCardinality(R, 4, None),
        ObjectMaxCardinality(R, 2, Some(ObjectIntersectionOf(C, D))))
      )

            EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C2)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C3)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C4)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C5)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C6)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C7)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C8)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C9)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C10)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C11)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C12)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C13)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C14)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C15)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C16)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C17)),
        ObjectSomeValuesFrom(R, C18),
        ObjectMaxCardinality(R, 1, Some(D))))

      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C2)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C3)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C4)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C5)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C6)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C7)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C8)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C9)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C10)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C11)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C12)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C13)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C14)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C15)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C16)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, C17)),
        ObjectSomeValuesFrom(R, C18),
        ObjectMaxCardinality(R, 1, Some(D))))
      EquivalentClassesAxiom(X, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R,
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, ObjectAllValuesFrom(invR, ObjectUnionOf(ObjectComplementOf(C), ObjectAllValuesFrom(R, C))))))))


[END] Extra test case 2

*/

  // TODO: add the "vegetarians" example from the conference talks.
//  ignore("vegetarians") {
//    val Animal = OWLClass(IRI(p, "Animal"))
//    val Vegetarian = OWLClass(IRI(p, "Vegetarian"))
//    val Vegetable = OWLClass(IRI(p, "Vegetable"))
//    val Edible = OWLClass(IRI(p, "Edible"))
//    val Person = OWLClass(IRI(p, "Person"))
//    val eats = ObjectProperty(IRI(p, "eats"))
//    val partOf = ObjectProperty(IRI(p, "partOf"))
//    val expected = Set(
//
//    )
//    val input = Set(
//      //SubClassOfAxiom(Animal, ObjectSomeValuesFrom(eats, Thing)),
//      //SubClassOfAxiom(Person, Animal),
//      //SubClassOfAxiom(Vegetarian,
//      //                           ObjectIntersectionOf(Set(Animal,
//      //                                                    ObjectAllValuesFrom(eats, ObjectComplementOf(Animal)),
//      //                                                    ObjectAllValuesFrom(eats, ObjectComplementOf(ObjectSomeValuesFrom(partOf, Animal))))))
//      //SubClassOfAxiom(Vegetable, ObjectIntersectionOf(Set(ObjectComplementOf(Animal), ObjectComplementOf(ObjectSomeValuesFrom(partOf, Animal)))))
//      // EquivalentClassesAxiom(Vegetable, ObjectIntersectionOf(Edible, ObjectComplementOf(ObjectSomeValuesFrom(partOf, Animal))))
//    )
//    val actual = transform(input)
//    assert(decorate(actual) === decorate(expected))
//  }

}
