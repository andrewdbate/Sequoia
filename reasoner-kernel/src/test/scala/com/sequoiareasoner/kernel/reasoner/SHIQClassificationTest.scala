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

package com.sequoiareasoner.kernel.reasoner

import com.sequoiareasoner.kernel.{OWLAxiomBuilder, CommonNames}
import org.scalatest.{FunSuite, RandomTestOrder}
import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.owl.model._

/** Tests to check that the computed taxonomy is both correct and complete for SHIQ ontologies.
  *
  * Some test cases are adapted from the HermiT reasoner.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class SHIQClassificationTest extends FunSuite with RandomTestOrder {
  import ClassificationTestUtils._
  import OWLAxiomBuilder._
  import CommonNames._

  // TODO: add the "vegetarians" example from the Sequoia conference talks.

  test("testClassificationSubClassBug simplified [adapted from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C1, ObjectExactCardinality(3, R, Thing))
    )
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  ignore("testClassificationSubClassBug [from HermiT]") { // FIXME: this test is too slow to pass
    val Petal = OWLClass(IRI(p, "Petal"))
    val DomainEntity = OWLClass(IRI(p, "DomainEntity"))
    val PetalShape = OWLClass(IRI(p, "PetalShape"))
    val Corolla = OWLClass(IRI(p, "Corolla"))
    val ps1 = OWLClass(IRI(p, "ps1"))
    val ps2 = OWLClass(IRI(p, "ps2"))
    val ps3 = OWLClass(IRI(p, "ps3"))
    val hasShape = ObjectProperty(IRI(p, "hasShape"))
    val hasPart = ObjectProperty(IRI(p, "hasPart"))
    val input = Set(
      SubClassOfAxiom(Petal, ObjectIntersectionOf(DomainEntity, ObjectSomeValuesFrom(hasShape, PetalShape))),
      SubClassOfAxiom(ps2, PetalShape),
      SubClassOfAxiom(PetalShape, DomainEntity),
      SubClassOfAxiom(Corolla, DomainEntity),
      SubClassOfAxiom(C1, ObjectIntersectionOf(Corolla, ObjectExactCardinality(4, hasPart, Petal))),
      SubClassOfAxiom(ps3, PetalShape),
      SubClassOfAxiom(C2, ObjectIntersectionOf(Corolla,
        ObjectAllValuesFrom(hasPart, ObjectUnionOf(ObjectComplementOf(Petal), ObjectAllValuesFrom(hasShape, ObjectUnionOf(ps1, ps2)))),
        ObjectExactCardinality(1, hasPart, ObjectIntersectionOf(Petal, ObjectSomeValuesFrom(hasShape, ps1))),
        ObjectExactCardinality(3, hasPart, ObjectIntersectionOf(Petal, ObjectSomeValuesFrom(hasShape, ps2))))),
      SubClassOfAxiom(ps1, PetalShape),
      EquivalentClassesAxiom(C4, ObjectIntersectionOf(Corolla, ObjectExactCardinality(4, hasPart, Petal))),
      DisjointClassesAxiom(ps1, ps2, ps3),
      DisjointClassesAxiom(Corolla, Petal, PetalShape),
      FunctionalObjectPropertyAxiom(hasShape)
    )
    // Expected result: class C2 is subsumed by C4.
    val expected = Set(
      SubClassOfAxiom(C4, Corolla),
      SubClassOfAxiom(ps3, PetalShape),
      SubClassOfAxiom(ps2, PetalShape),
      SubClassOfAxiom(ps1, PetalShape),
      SubClassOfAxiom(Petal, DomainEntity),
      SubClassOfAxiom(PetalShape, DomainEntity),
      SubClassOfAxiom(Corolla, DomainEntity),
      SubClassOfAxiom(C1, C4),
      SubClassOfAxiom(C2, C4)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox2 simplified [adapted from HermiT]") {
    // Tests incoherency caused by number restrictions.
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(2, R), ObjectMaxCardinality(1, R)))
    )
    // Expected result: A is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox2 [from HermiT]") {
    val Complex1 = OWLClass(IRI(p, "Complex1"))
    val Complex2 = OWLClass(IRI(p, "Complex2"))
    // Tests incoherency caused by number restrictions.
    val input = Set(
      DisjointClassesAxiom(C, D),
      EquivalentClassesAxiom(Complex1, ObjectIntersectionOf(ObjectMinCardinality(2, R), ObjectMaxCardinality(1, R))),
      EquivalentClassesAxiom(Complex2, ObjectIntersectionOf(ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D)))
    )
    // Expected result: classes Complex1 and Complex2 are unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, Complex1, Complex2)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox3c [from HermiT]") {
    val Complex1 = OWLClass(IRI(p, "Complex1"))
    val t1 = ObjectProperty(IRI(p, "t1"))
    val tc = ObjectProperty(IRI(p, "tc"))
    val td = ObjectProperty(IRI(p, "td"))
    // Tests incoherency caused by the role hierarchy and number restrictions.
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(C, D)),
      SubObjectPropertyOfAxiom(t1, tc),
      SubObjectPropertyOfAxiom(t1, td),
      SubClassOfAxiom(Thing, ObjectAllValuesFrom(tc, C)),
      SubClassOfAxiom(Thing, ObjectAllValuesFrom(td, D)),
      SubObjectPropertyOfAxiom(tc, R),
      SubObjectPropertyOfAxiom(td, S),
      EquivalentClassesAxiom(Complex1, ObjectIntersectionOf(ObjectAllValuesFrom(t1, A), ObjectMinCardinality(3, t1), ObjectMaxCardinality(1, R), ObjectMaxCardinality(1, S)))
    )
    // Expected result: class Complex1 is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, Complex1),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox3cIrh [from HermiT]") {
    val Complex1 = OWLClass(IRI(p, "Complex1"))
    // Tests incoherency caused by number restrictions.
    val input = Set(
      DisjointClassesAxiom(C, D),
      EquivalentClassesAxiom(A, ObjectUnionOf(C, D)),
      EquivalentClassesAxiom(Complex1, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)))
    )
    // Expected result: class Complex1 is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, Complex1),
      SubClassOfAxiom(C, A),
      SubClassOfAxiom(D, A)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  ignore("testHeinsohnTBox3 [from HermiT]") { // FIXME: slow
    val tt = ObjectProperty(IRI(p, "tt"))
    val t1 = ObjectProperty(IRI(p, "t1"))
    val t2 = ObjectProperty(IRI(p, "t2"))
    val t3 = ObjectProperty(IRI(p, "t3"))
    val Complex1a = OWLClass(IRI(p, "Complex1a"))
    val Complex1b = OWLClass(IRI(p, "Complex1b"))
    val Complex2a = OWLClass(IRI(p, "Complex2a"))
    val Complex2b = OWLClass(IRI(p, "Complex2b"))
    val Complex3a = OWLClass(IRI(p, "Complex3a"))
    val Complex3b = OWLClass(IRI(p, "Complex3b"))
    val Complex4a = OWLClass(IRI(p, "Complex4a"))
    val Complex4b = OWLClass(IRI(p, "Complex4b"))
    // Tests incoherency caused by number restrictions and role hierarchy.
    val input = Set(
      DisjointClassesAxiom(C, D, E),
      SubClassOfAxiom(A, ObjectUnionOf(C, D)),
      SubObjectPropertyOfAxiom(R1, R),
      SubObjectPropertyOfAxiom(R2, R),
      SubObjectPropertyOfAxiom(R3, R),
      SubObjectPropertyOfAxiom(t1, tt),
      SubObjectPropertyOfAxiom(t2, tt),
      SubObjectPropertyOfAxiom(t3, tt),
      EquivalentClassesAxiom(Complex1a, ObjectIntersectionOf(ObjectMinCardinality(1, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))),
      EquivalentClassesAxiom(Complex1b, ObjectMinCardinality(2, R)),
      EquivalentClassesAxiom(Complex2a, ObjectIntersectionOf(ObjectMaxCardinality(2, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))),
      EquivalentClassesAxiom(Complex2b, ObjectIntersectionOf(ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D))),
      EquivalentClassesAxiom(Complex3a, ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectMinCardinality(3, R), ObjectMaxCardinality(1, R, C))),
      EquivalentClassesAxiom(Complex3b, ObjectMinCardinality(2, R, D)),
      EquivalentClassesAxiom(Complex4a, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R1, ObjectIntersectionOf(ObjectMaxCardinality(1, tt), ObjectSomeValuesFrom(t1, C))),
        ObjectSomeValuesFrom(R2, ObjectIntersectionOf(ObjectMaxCardinality(1, tt), ObjectSomeValuesFrom(t2, D))),
        ObjectSomeValuesFrom(R2, ObjectIntersectionOf(ObjectMaxCardinality(1, tt), ObjectSomeValuesFrom(t2, D))),
        ObjectSomeValuesFrom(R3, ObjectIntersectionOf(ObjectMaxCardinality(1, tt), ObjectSomeValuesFrom(t3, E)))
      )),
      EquivalentClassesAxiom(Complex4b, ObjectMinCardinality(2, R))
    )
    // Expected result: complex1a subsumed by complex1b; complex2a subsumed by complex2b; complex3a subsumed by complex3b; complex4a subsumed by complex4b.
    val expected = Set(
      EquivalentClassesAxiom(Complex1b, Complex4b),
      SubClassOfAxiom(Complex1a, Complex1b),
      SubClassOfAxiom(Complex4a, Complex1b),
      SubClassOfAxiom(Complex3b, Complex1b),
      SubClassOfAxiom(Complex3a, Complex3b),
      SubClassOfAxiom(Complex2a, Complex1a),
      SubClassOfAxiom(Complex2a, Complex2b)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox3 simplified [modified from HermiT]") {
    val Complex2a = OWLClass(IRI(p, "Complex2a"))
    val Complex2b = OWLClass(IRI(p, "Complex2b"))
    // Tests incoherency caused by number restrictions and role hierarchy.
    val input = Set(
      DisjointClassesAxiom(C, D),
      SubClassOfAxiom(Complex2a, ObjectIntersectionOf(ObjectMaxCardinality(2, R), ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))),
      SubClassOfAxiom(ObjectIntersectionOf(ObjectMaxCardinality(1, R, C), ObjectMaxCardinality(1, R, D)), Complex2b)
    )
    // Expected result: complex2a subsumed by complex2b.
    val expected = Set(
      SubClassOfAxiom(Complex2a, Complex2b)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox3Modified [from HermiT]") {
    val input = Set(
      DisjointClassesAxiom(C, D),
      SubClassOfAxiom(A, ObjectMaxCardinality(2, R)),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(Thing, ObjectUnionOf(ObjectMinCardinality(2, R, C), ObjectMinCardinality(2, R, D), B))
    )
    // Expected result: class A is subsumed by B.
    val expected = Set(
      SubClassOfAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox4b [from HermiT]") {
    val input = Set(
      DisjointClassesAxiom(C, D),
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(ObjectMinCardinality(2, S)), C)), ObjectAllValuesFrom(R, D))),
      EquivalentClassesAxiom(X2, ObjectAllValuesFrom(R, ObjectMaxCardinality(1, S)))
    )
    // Expected result: class X1 is subsumed by X2.
    val expected = Set(
      SubClassOfAxiom(X1, X2)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT1a [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C1, ObjectComplementOf(ObjectUnionOf(C2, C3, C4, C5))),
      SubClassOfAxiom(C2, ObjectComplementOf(ObjectUnionOf(C3, C4, C5))),
      SubClassOfAxiom(C3, ObjectComplementOf(ObjectUnionOf(C4, C5))),
      SubClassOfAxiom(C4, ObjectComplementOf(C5)),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectSomeValuesFrom(R, C2), ObjectSomeValuesFrom(R, C3), ObjectMaxCardinality(2, R)))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT1b [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C1, ObjectComplementOf(ObjectUnionOf(C2, C3, C4, C5))),
      SubClassOfAxiom(C2, ObjectComplementOf(ObjectUnionOf(C3, C4, C5))),
      SubClassOfAxiom(C3, ObjectComplementOf(ObjectUnionOf(C4, C5))),
      SubClassOfAxiom(C4, ObjectComplementOf(C5)),
      EquivalentClassesAxiom(X, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R, C1))))
    )
    // Expected result: class X is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT1c [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C1, ObjectComplementOf(ObjectUnionOf(C2, C3, C4, C5))),
      SubClassOfAxiom(C2, ObjectComplementOf(ObjectUnionOf(C3, C4, C5))),
      SubClassOfAxiom(C3, ObjectComplementOf(ObjectUnionOf(C4, C5))),
      SubClassOfAxiom(C4, ObjectComplementOf(C5)),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(C2, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1), ObjectMaxCardinality(1, R)))))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  ignore("testIanT3 [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C1, ObjectComplementOf(ObjectUnionOf(C2, C3, C4, C5))),
      SubClassOfAxiom(C2, ObjectComplementOf(ObjectUnionOf(C3, C4, C5))),
      SubClassOfAxiom(C3, ObjectComplementOf(ObjectUnionOf(C4, C5))),
      SubClassOfAxiom(C4, ObjectComplementOf(C5)),

      EquivalentClassesAxiom(X1, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectMaxCardinality(3, R))),

      EquivalentClassesAxiom(X2, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectMaxCardinality(3, R))),

      EquivalentClassesAxiom(X3, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectMaxCardinality(3, R))),

      EquivalentClassesAxiom(X4, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, C1),
        ObjectSomeValuesFrom(R, C2),
        ObjectSomeValuesFrom(R, C3),
        ObjectSomeValuesFrom(R, C4),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C1, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C3, C)),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C4, C)),
        ObjectMaxCardinality(4, R))),

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
        ObjectMaxCardinality(4, R))),

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
        ObjectMaxCardinality(5, R)))
    )
    // Expected result: X1, X4 and X6 are satisfiable, whereas X2, X3 and X5 are unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X2, X3, X5)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT11 [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(S, invS),
      SubObjectPropertyOfAxiom(S, R),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectComplementOf(C), ObjectMaxCardinality(1, R), ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invS, C)), ObjectSomeValuesFrom(S, C)))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT12 [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(S, ObjectIntersectionOf(ObjectComplementOf(C), ObjectComplementOf(D))),
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectMaxCardinality(1, invR), ObjectSomeValuesFrom(invR, ObjectAllValuesFrom(S, C)))))
      )
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  ignore("testIanBug3 [from HermiT]") {
    val input = Set(
      EquivalentClassesAxiom(X, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, A),
        ObjectMinCardinality(3, R, C),
        ObjectMinCardinality(3, R, D),
        ObjectMinCardinality(2, R, ObjectIntersectionOf(E, ObjectComplementOf(ObjectIntersectionOf(C, D)))),
        ObjectMaxCardinality(4, R),
        ObjectMaxCardinality(2, R, ObjectIntersectionOf(C, D))
      ))
    )
    // Expected result: class X is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  ignore("testIanBug3 simplified [adapted from HermiT]") {
    val input = Set(
      SubClassOfAxiom(X, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, A),
        ObjectMinCardinality(3, R, C),
        ObjectMinCardinality(3, R, D),
        ObjectMinCardinality(2, R, ObjectComplementOf(ObjectIntersectionOf(C, D))),
        ObjectMaxCardinality(4, R),
        ObjectMaxCardinality(2, R, ObjectIntersectionOf(C, D))
      ))
    )
    // Expected result: class X is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBug7 [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(A, ObjectComplementOf(B)),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, A), ObjectMaxCardinality(1, R, A), ObjectSomeValuesFrom(R, B), ObjectMaxCardinality(1, R, B)))
    )
    // Expected result: class X is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBug8 [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(X, ObjectComplementOf(Y)),
      SubClassOfAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(1, R, X), ObjectMaxCardinality(1, R, X))),
      SubClassOfAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(1, R, Y), ObjectMaxCardinality(1, R, Y)))
    )
    // Expected result: class A is satisfiable
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanMergeTest1 [from HermiT]") {
    // If universals are not clausified in a way that preserves Horn wherever possible, then this test will not
    // terminate in any reasonable amount of time.
    val input = Set(
      InverseObjectPropertiesAxiom(R, S),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(S, ObjectComplementOf(D)))),
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
        ObjectMaxCardinality(1, R, D)))
    )
    // Expected result: class X is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanMergeTest2 [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, S),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(S, D))),
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
        ObjectMaxCardinality(1, R, D)))
    )
    val expected = Set(
      SubClassOfAxiom(C, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanQNRTest [from HermiT]") { // TODO: this used to be slow. Check the trace to see which optimisations is being used in this example.
    val input = Set(
      SubObjectPropertyOfAxiom(son, child),
      SubObjectPropertyOfAxiom(daughter, child),
      EquivalentClassesAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(2, son, male), ObjectMinCardinality(2, daughter, ObjectComplementOf(male)))),
      EquivalentClassesAxiom(B, ObjectMinCardinality(4, child))
    )
    val expected = Set(
      SubClassOfAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanFact3 [from HermiT]") {
    val input = Set(
      FunctionalObjectPropertyAxiom(F1),
      FunctionalObjectPropertyAxiom(F2),
      FunctionalObjectPropertyAxiom(F3),
      SubObjectPropertyOfAxiom(F3, F1),
      SubObjectPropertyOfAxiom(F3, F2),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(F1, C1), ObjectSomeValuesFrom(F2, ObjectComplementOf(C1)), ObjectSomeValuesFrom(F3, C2)))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanFact4 [from HermiT]") {
    val rx = ObjectProperty(IRI(p, "rx"))
    val rxa = ObjectProperty(IRI(p, "rxa"))
    val rx1 = ObjectProperty(IRI(p, "rx1"))
    val rx1a = ObjectProperty(IRI(p, "rx1a"))
    val rx2 = ObjectProperty(IRI(p, "rx2"))
    val rx2a = ObjectProperty(IRI(p, "rx2a"))
    val rx3 = ObjectProperty(IRI(p, "rx3"))
    val rx3a = ObjectProperty(IRI(p, "rx3a"))
    val rx4 = ObjectProperty(IRI(p, "rx4"))
    val rx4a = ObjectProperty(IRI(p, "rx4a"))
    val input = Set(
      FunctionalObjectPropertyAxiom(rx),
      FunctionalObjectPropertyAxiom(rx3),
      SubObjectPropertyOfAxiom(rx3, rx),
      SubObjectPropertyOfAxiom(rx3, rx1),
      FunctionalObjectPropertyAxiom(rx4),
      SubObjectPropertyOfAxiom(rx4, rx),
      SubObjectPropertyOfAxiom(rx4, rx2),
      FunctionalObjectPropertyAxiom(rx3a),
      SubObjectPropertyOfAxiom(rx3a, rxa),
      SubObjectPropertyOfAxiom(rx3a, rx1a),
      FunctionalObjectPropertyAxiom(rx4a),
      SubObjectPropertyOfAxiom(rx4a, rxa),
      SubObjectPropertyOfAxiom(rx4a, rx2a),
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(ObjectSomeValuesFrom(rx3, C1), ObjectSomeValuesFrom(rx4, C2))),
      EquivalentClassesAxiom(X2, ObjectSomeValuesFrom(rx3, ObjectIntersectionOf(C1, C2))),
      EquivalentClassesAxiom(X3, ObjectIntersectionOf(ObjectSomeValuesFrom(rx3a, C1), ObjectSomeValuesFrom(rx4a, C2))),
      EquivalentClassesAxiom(X4, ObjectSomeValuesFrom(rx3a, ObjectIntersectionOf(C1, C2)))
    )
    // Expected result: X1 is subsumed by X2, and X3 is not subsumed but X4.
    val expected = Set(
      SubClassOfAxiom(X1, X2)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT2 [from HermiT]") {
    val input = Set(
      SubObjectPropertyOfAxiom(R, F1),
      SubObjectPropertyOfAxiom(R, F2),
      SubClassOfAxiom(C1, ObjectComplementOf(C2)),
      FunctionalObjectPropertyAxiom(F1),
      FunctionalObjectPropertyAxiom(F2),
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(ObjectSomeValuesFrom(F1, C1), ObjectSomeValuesFrom(F2, C2))),
      EquivalentClassesAxiom(X2, ObjectIntersectionOf(ObjectSomeValuesFrom(F1, C1), ObjectSomeValuesFrom(F2, C2), ObjectSomeValuesFrom(R, Thing)))
    )
    // Expected result: X1 is satisfiable, whereas X2 are unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X2)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT5 [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(F0, invF0),
      TransitiveObjectPropertyAxiom(R),
      SubObjectPropertyOfAxiom(F0, R),
      FunctionalObjectPropertyAxiom(F0),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectComplementOf(A), ObjectSomeValuesFrom(invF0, A), ObjectAllValuesFrom(invR, ObjectSomeValuesFrom(invF0, A))))
    )
    // Expected result: class X is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT6 [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(F0, invF0),
      TransitiveObjectPropertyAxiom(R),
      SubObjectPropertyOfAxiom(F0, R),
      FunctionalObjectPropertyAxiom(F0),
      EquivalentClassesAxiom(D, ObjectIntersectionOf(C, ObjectSomeValuesFrom(F0, ObjectComplementOf(C)))),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectComplementOf(C), ObjectSomeValuesFrom(invF0, D), ObjectAllValuesFrom(invR, ObjectSomeValuesFrom(invF0, D))))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X),
      SubClassOfAxiom(D, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT7a [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(F0, invF0),
      TransitiveObjectPropertyAxiom(R),
      FunctionalObjectPropertyAxiom(F0),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, ObjectAllValuesFrom(invR, ObjectComplementOf(C)))))))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT7b [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(F0, invF0),
      TransitiveObjectPropertyAxiom(R),
      FunctionalObjectPropertyAxiom(F0),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R,
        ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, ObjectAllValuesFrom(invR, ObjectUnionOf(ObjectComplementOf(C), ObjectAllValuesFrom(R, C))))))))
    )
    // Expected result: class X is satisfiable.
    val expected = Set(
      SubClassOfAxiom(X, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT7c [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(F0, invF0),
      TransitiveObjectPropertyAxiom(R),
      FunctionalObjectPropertyAxiom(F0),
      EquivalentClassesAxiom(X, ObjectSomeValuesFrom(F0, ObjectIntersectionOf(C, ObjectAllValuesFrom(invF0, ObjectSomeValuesFrom(F0, ObjectComplementOf(C))))))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT9 [from HermiT]") {
    val successor = ObjectProperty(IRI(p, "successor"))
    val invsuccessor = ObjectProperty(IRI(p, "invsuccessor"))
    val descendant = ObjectProperty(IRI(p, "descendant"))
    val root = OWLClass(IRI(p, "root"))
    val node = OWLClass(IRI(p, "node"))
    val InfiniteTreeRoot = OWLClass(IRI(p, "InfiniteTreeRoot"))
    val InfiniteTreeNode = OWLClass(IRI(p, "InfiniteTreeNode"))
    val input = Set(
      InverseObjectPropertiesAxiom(successor, invsuccessor),
      TransitiveObjectPropertyAxiom(descendant),
      SubObjectPropertyOfAxiom(successor, descendant),
      InverseFunctionalObjectPropertyAxiom(successor),
      SubClassOfAxiom(root, ObjectComplementOf(ObjectSomeValuesFrom(invsuccessor, Thing))),
      SubClassOfAxiom(InfiniteTreeNode, ObjectIntersectionOf(node, ObjectSomeValuesFrom(successor, InfiniteTreeNode))),
      SubClassOfAxiom(InfiniteTreeRoot, ObjectIntersectionOf(InfiniteTreeNode, root)),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(InfiniteTreeRoot, ObjectAllValuesFrom(descendant, ObjectSomeValuesFrom(invsuccessor, root))))
    )
    // Expected result: InfiniteTreeRoot is satisfiable, whereas X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X),
      SubClassOfAxiom(InfiniteTreeRoot, root),
      SubClassOfAxiom(InfiniteTreeRoot, InfiniteTreeNode),
      SubClassOfAxiom(InfiniteTreeNode, node)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT10 [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(S, invS),
      InverseObjectPropertiesAxiom(F0, invF0),
      InverseObjectPropertiesAxiom(F1, invF1),
      FunctionalObjectPropertyAxiom(F0),
      FunctionalObjectPropertyAxiom(F1),
      SubObjectPropertyOfAxiom(S, F0),
      SubObjectPropertyOfAxiom(S, F1),
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(ObjectComplementOf(C), ObjectSomeValuesFrom(F0, ObjectIntersectionOf(ObjectAllValuesFrom(invS, C), ObjectAllValuesFrom(invF0, ObjectSomeValuesFrom(S, C)))))),
      EquivalentClassesAxiom(X2, ObjectIntersectionOf(ObjectAllValuesFrom(S, ObjectComplementOf(C)), ObjectSomeValuesFrom(S, ObjectIntersectionOf(C, ObjectSomeValuesFrom(invS, C))))),
      EquivalentClassesAxiom(X3, ObjectIntersectionOf(ObjectSomeValuesFrom(F0, C), ObjectSomeValuesFrom(F1, ObjectComplementOf(C)))),
      EquivalentClassesAxiom(X4, ObjectIntersectionOf(ObjectSomeValuesFrom(F0, C), ObjectSomeValuesFrom(S, Thing), ObjectSomeValuesFrom(F1, ObjectComplementOf(C)))),
      EquivalentClassesAxiom(X5, ObjectIntersectionOf(ObjectSomeValuesFrom(F1, C), ObjectSomeValuesFrom(F1, ObjectIntersectionOf(ObjectComplementOf(C), ObjectAllValuesFrom(invF1, ObjectSomeValuesFrom(S, Thing))))))
    )
    // Expected result: X3 is satisfiable, whereas X1, X2, X4 and X5 are unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X1, X2, X4, X5)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("bug in old calculus") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(ObjectInverseOf(R), B)),
      SubClassOfAxiom(B, ObjectUnionOf(C, D)),
      FunctionalObjectPropertyAxiom(R),
      SubClassOfAxiom(D, ObjectMinCardinality(2, R)),
      SubClassOfAxiom(ObjectSomeValuesFrom(ObjectInverseOf(R), C), E)
    )
    val expected = Set(
      EquivalentClassesAxiom(D, Nothing),
      SubClassOfAxiom(B, C),
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Self-Reflexivity 1"){
    val input = Set(
      SubClassOfAxiom(A, ObjectHasSelf( R )),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, A), B),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      SubClassOfAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Self-Reflexivity 2"){
    val input = Set(
      SubClassOfAxiom(A, ObjectHasSelf(R)),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      FunctionalObjectPropertyAxiom(R),
    )
    val expected = Set(
      SubClassOfAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Self-Reflexivity 3"){
    val input = Set(
      SubClassOfAxiom(A, ObjectHasSelf(R)),
      SubClassOfAxiom(ObjectHasSelf(S), B),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      SubClassOfAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

}
