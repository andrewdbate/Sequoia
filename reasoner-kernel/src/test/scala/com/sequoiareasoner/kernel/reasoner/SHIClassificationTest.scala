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

/** Tests to check that the computed taxonomy is both correct and complete for SHI ontologies.
  *
  * Some test cases are adapted from the HermiT reasoner.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class SHIClassificationTest extends FunSuite with RandomTestOrder {
  import ClassificationTestUtils._
  import OWLAxiomBuilder._
  import CommonNames._

  // TODO: get a trace of the clauses derived during the run of the calculus and compare it to an expected set.

  test("obviously inconsistent [new]") {
    val input = Set(
      SubClassOfAxiom(A, Nothing),
      SubClassOfAxiom(Thing, C),
      SubClassOfAxiom(C, A)
    )
    computeTaxonomy(input) match {
      case None =>
        // This is the correct result
      case Some(actual) =>
        fail(s"Ontology should have been inconsistent!")
    }
  }

  ignore("testUniversalRoleSubsumption [from HermiT]") { // FIXME: handle topObjectProperty correctly
    val Ax2 = OWLClass(IRI(p, "Ax2"))
    val Ax3 = OWLClass(IRI(p, "Ax3"))
    val input = Set(
      SubClassOfAxiom(C, D),
      EquivalentClassesAxiom(Ax2, ObjectAllValuesFrom(topObjectProperty, ObjectUnionOf(ObjectComplementOf(D), E))),
      EquivalentClassesAxiom(Ax3, ObjectAllValuesFrom(topObjectProperty, ObjectUnionOf(ObjectComplementOf(C), E)))
    )
    // Expected result: class Ax2 is subsumed by Ax3.
    val expected = Set(
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(Ax2, Ax3)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testWidmann1 modified 1 [adapted from HermiT]") {
    val input = Set(
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, ObjectComplementOf(C))),
      SubClassOfAxiom(Thing, ObjectAllValuesFrom(ObjectInverseOf(R), ObjectAllValuesFrom(R, C)))
    )
    computeTaxonomy(input) match {
      case None =>
      // This is the correct result (if there were an ABox, it too should be unsatisfiable).
      case Some(actual) =>
        fail(s"Ontology should have been inconsistent!")
    }
  }

  test("testWidmann1 modified 2 [adapted from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, ObjectComplementOf(C))),
      SubClassOfAxiom(Thing, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R, C)))
    )
    computeTaxonomy(input) match {
      case None =>
      // This is the correct result (if there were an ABox, it too should be unsatisfiable).
      case Some(actual) =>
        fail(s"Ontology should have been inconsistent!")
    }
  }

  test("testWidmann1 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(S, invS),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(R, ObjectSomeValuesFrom(R, ObjectComplementOf(C))))),
      SubClassOfAxiom(Thing, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(S, ObjectAllValuesFrom(invS, C)))))
    )
    computeTaxonomy(input) match {
      case None =>
      // This is the correct result (if there were an ABox, it too should be unsatisfiable).
      case Some(actual) =>
        fail(s"Ontology should have been inconsistent!")
    }
  }

  test("testWidmann2 simplified [adapted from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, Thing)),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectAllValuesFrom(R, C))),
      EquivalentClassesAxiom(X, ObjectSomeValuesFrom(ObjectInverseOf(R), ObjectComplementOf(C)))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X),
      EquivalentClassesAxiom(Thing, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testWidmann2 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, D)),
      InverseObjectPropertiesAxiom(R, invR),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(invR, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, C)))))),
      EquivalentClassesAxiom(X, ObjectSomeValuesFrom(invR, ObjectComplementOf(C)))
    )
    // Expected result: class X is unsatisfiable
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X),
      EquivalentClassesAxiom(Thing, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testWidmann3 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(invR, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(R, ObjectSomeValuesFrom(invR, ObjectComplementOf(C)))))),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(invR, ObjectSomeValuesFrom(R, C))),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(invR, ObjectAllValuesFrom(invR, ObjectSomeValuesFrom(invR, ObjectSomeValuesFrom(invR, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, C))))))),
      SubClassOfAxiom(Thing, ObjectAllValuesFrom(R, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R, C)))))))
    )
    computeTaxonomy(input) match {
      case None =>
      // This is the correct result (if there were an ABox, it too should be unsatisfiable).
      case Some(actual) =>
        fail(s"Ontology should have been inconsistent!")
    }
  }

  test("testSubsumption1 [from HermiT]") {
    val Dog = OWLClass(IRI(p, "Dog"))
    val Animal = OWLClass(IRI(p, "Animal"))
    val Person = OWLClass(IRI(p, "Person"))
    val Student = OWLClass(IRI(p, "Student"))
    val input = Set(
      SubClassOfAxiom(Dog, Animal),
      SubClassOfAxiom(Person, Animal),
      SubClassOfAxiom(Student, Person)
    )
    // Expected result: Student is subsumed by Animal; Animal not subsumbed by Student; Student not subsumed by Dog; Dog is not subsumed by Student.
    val expected = Set(
      SubClassOfAxiom(Dog, Animal),
      SubClassOfAxiom(Person, Animal),
      SubClassOfAxiom(Student, Person)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testSubsumption2 [from HermiT]") {
    val input = Set(
      SubObjectPropertyOfAxiom(R, S),
      EquivalentClassesAxiom(A, ObjectSomeValuesFrom(R, C)),
      EquivalentClassesAxiom(B, ObjectSomeValuesFrom(S, C))
    )
    // Expected result: A is subsumed by B, but B is not subsumed by A.
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

  test("testSubsumption3 [from HermiT]") {
    val input = Set(
      EquivalentObjectPropertiesAxiom(Set(R, S)),
      EquivalentClassesAxiom(A, ObjectSomeValuesFrom(R, C)),
      EquivalentClassesAxiom(B, ObjectSomeValuesFrom(S, C))
    )
    // Expected result: classes A and B are equivalent.
    val expected = Set(
      EquivalentClassesAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox1 [from HermiT]") {
    val Complex1 = OWLClass(IRI(p, "Complex1"))
    val Complex2 = OWLClass(IRI(p, "Complex2"))
    val Complex3 = OWLClass(IRI(p, "Complex3"))
    val E3 = OWLClass(IRI(p, "E3"))
    val D1 = OWLClass(IRI(p, "D1"))
    // Tests incoherency caused by disjoint concepts.
    val input = Set(
      DisjointClassesAxiom(C, D),
      DisjointClassesAxiom(C1, D1),
      SubClassOfAxiom(E3, C),
      SubClassOfAxiom(F, D),
      SubClassOfAxiom(C1, D1),
      EquivalentClassesAxiom(Complex1, ObjectIntersectionOf(C, D)),
      EquivalentClassesAxiom(Complex2, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectIntersectionOf(C, D)), ObjectSomeValuesFrom(R, Thing))),
      EquivalentClassesAxiom(Complex3, ObjectIntersectionOf(E3, F))
    )
    // Expected result: classes Complex1, Complex2, Complex3, and C1 are unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, C1, Complex1, Complex2, Complex3),
      SubClassOfAxiom(F, D),
      SubClassOfAxiom(E3, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testHeinsohnTBox4a [from HermiT]") {
    // This example creates a cycle in the context structure.
    // Tests role restrictions.
    val input = Set(
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(ObjectAllValuesFrom(R, D), ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(D), E)))),
      EquivalentClassesAxiom(X2, ObjectAllValuesFrom(R, E))
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

  test("testHeinsohnTBox7 [from HermiT]") {
    // Tests inverse roles.
    val input = Set(
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectAllValuesFrom(ObjectInverseOf(R), A)), ObjectSomeValuesFrom(R, Thing)))
    )
    // Expected result: class X is subsumed by A.
    val expected = Set(
      SubClassOfAxiom(X, A)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanT4 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      TransitiveObjectPropertyAxiom(P),
      InverseObjectPropertiesAxiom(R, invR),
      InverseObjectPropertiesAxiom(P, invP),
      InverseObjectPropertiesAxiom(S, invS),
      EquivalentClassesAxiom(C, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(invP, ObjectAllValuesFrom(invS, ObjectComplementOf(A))))),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(A, ObjectSomeValuesFrom(S, ObjectIntersectionOf(
        ObjectSomeValuesFrom(R, Thing),
        ObjectSomeValuesFrom(P, Thing),
        ObjectAllValuesFrom(R, C),
        ObjectAllValuesFrom(P, ObjectSomeValuesFrom(R, Thing)),
        ObjectAllValuesFrom(P, ObjectSomeValuesFrom(P, Thing)),
        ObjectAllValuesFrom(P, ObjectAllValuesFrom(R, C))))))
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

  test("testIanT8a simplified [adapted from HermiT]") {
    val input = Set(
      SubClassOfAxiom(X, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, C)))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, C))), X)
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

  test("testIanT8a [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R1, C))),
                                                     ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R1, ObjectComplementOf(C))))))
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

  test("testIanT8 [from HermiT]") {
    val input = Set(
      InverseObjectPropertiesAxiom(R, invR),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R1, Thing),
                                                     ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R1, C))),
                                                     ObjectSomeValuesFrom(R, ObjectAllValuesFrom(invR, ObjectAllValuesFrom(R1, ObjectComplementOf(C))))))

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

  test("testIanT13 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val a = OWLClass(IRI(p, "A"))
    val a1 = OWLClass(IRI(p, "A1"))
    val a2 = OWLClass(IRI(p, "A2"))
    val a3a = OWLClass(IRI(p, "A3a"))
    val a3b = OWLClass(IRI(p, "A3b"))
    val a3c = OWLClass(IRI(p, "A3c"))
    val a3e = OWLClass(IRI(p, "A3e"))
    val input = Set(
      InverseObjectPropertiesAxiom(S, invS),
      EquivalentClassesAxiom(a1, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(invS, ObjectAllValuesFrom(R, C)))),
      EquivalentClassesAxiom(a2, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(invS, ObjectAllValuesFrom(R, ObjectComplementOf(C))))),
      EquivalentClassesAxiom(a3a, ObjectSomeValuesFrom(S, ObjectAllValuesFrom(invS, ObjectUnionOf(ObjectSomeValuesFrom(R, D), ObjectSomeValuesFrom(S, D))))),
      EquivalentClassesAxiom(a3b, ObjectUnionOf(ObjectSomeValuesFrom(R, D), ObjectSomeValuesFrom(S, D))),
      EquivalentClassesAxiom(a3c, ObjectUnionOf(ObjectSomeValuesFrom(R, D), D)),
      EquivalentClassesAxiom(a3e, ObjectSomeValuesFrom(R, D)),
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(a3a, a2, a1)),
      EquivalentClassesAxiom(X2, ObjectIntersectionOf(a3b, a2, a1)),
      EquivalentClassesAxiom(X3, ObjectIntersectionOf(a3c, a2, a1)),
      EquivalentClassesAxiom(X4, ObjectIntersectionOf(a3e, a2, a1)),
      EquivalentClassesAxiom(X5, ObjectIntersectionOf(a, a2, a1)),
      EquivalentClassesAxiom(X6, ObjectIntersectionOf(ObjectIntersectionOf(a3a, a2, a1), ObjectComplementOf(ObjectIntersectionOf(a3b, a2, a1)))),
      EquivalentClassesAxiom(X7, ObjectIntersectionOf(ObjectComplementOf(ObjectIntersectionOf(a3a, a2, a1)), ObjectIntersectionOf(a3b, a2, a1))),
      EquivalentClassesAxiom(X8, ObjectIntersectionOf(ObjectIntersectionOf(a3c, a2, a1), ObjectComplementOf(ObjectIntersectionOf(a3c, a2, a1))))
    )
    // Expected result: X1, X2, X3 and X5 are satisfiable, whereas X4, X6, X7 and X8 are unsatisfiable.
    val expected = Set(
      SubClassOfAxiom(a3a, a3b),
      SubClassOfAxiom(a3e, a3b),
      SubClassOfAxiom(a3e, a3c),
      SubClassOfAxiom(D, a3c),
      EquivalentClassesAxiom(X1, X2),
      SubClassOfAxiom(X2, a1),
      SubClassOfAxiom(X2, a2),
      SubClassOfAxiom(X2, a3a),
      SubClassOfAxiom(X3, a1),
      SubClassOfAxiom(X3, a2),
      SubClassOfAxiom(X3, D),
      SubClassOfAxiom(X5, a),
      SubClassOfAxiom(X5, a2),
      SubClassOfAxiom(X5, a1),
      EquivalentClassesAxiom(Nothing, X4, X6, X7, X8)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanFact1 [from HermiT]") {
    val input = Set(
      DisjointClassesAxiom(A, B, C),
      EquivalentClassesAxiom(X, ObjectUnionOf(ObjectIntersectionOf(A, B), ObjectIntersectionOf(A, C), ObjectIntersectionOf(B, C)))
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

  test("testIanFact2 [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C, ObjectAllValuesFrom(R, C)),
      SubClassOfAxiom(ObjectAllValuesFrom(R, C), D)
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

  test("testIanBug1b [from HermiT]") {
    val input = Set(
      EquivalentClassesAxiom(C, ObjectIntersectionOf(A, ObjectComplementOf(B))),
      SubClassOfAxiom(A, ObjectIntersectionOf(D, ObjectComplementOf(C))),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectComplementOf(C), A, ObjectComplementOf(B), D))
    )
    // Expected result: class X is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBug4 simplified [adapted from HermiT]") {
    // The handling of the RBox has to find that R is symmetric.
    val input = Set(
      InverseObjectPropertiesAxiom(R, S),
      SubObjectPropertyOfAxiom(R, S),
      TransitiveObjectPropertyAxiom(R),
      SubClassOfAxiom(X, C),
      SubClassOfAxiom(X, ObjectSomeValuesFrom(R, Thing)),
      SubClassOfAxiom(Thing, ObjectAllValuesFrom(R, ObjectComplementOf(C)))
    )
    // Expected result: X is unsatisfiable.
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

  test("testIanBug4 [from HermiT]") {
    // The handling of the RBox has to find that R is reflexive.
    val input = Set(
      InverseObjectPropertiesAxiom(R, S),
      SubObjectPropertyOfAxiom(R, S),
      TransitiveObjectPropertyAxiom(R),
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, Thing), ObjectAllValuesFrom(R, ObjectComplementOf(C)))),
      EquivalentClassesAxiom(X2, ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, C)), ObjectAllValuesFrom(R, ObjectComplementOf(C))))
    )
    // Expected result: both X1 and X2 are unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X1, X2)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBug5 [from HermiT]") {
    val input = Set(
      TransitiveObjectPropertyAxiom(R1),
      SubObjectPropertyOfAxiom(R2, R1),
      TransitiveObjectPropertyAxiom(R2),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectAllValuesFrom(R1, C), ObjectSomeValuesFrom(R2, ObjectSomeValuesFrom(R1, ObjectComplementOf(C)))))
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

  test("testIanBug6 simplified [adapted from HermiT]") {
    /* Tests that an automaton is constructed for R, since R is a non-simple property.
     * If the role inclusion S1 -> R were be to translated into a clause directly as S1(x,z) -> R(x,z), then X1 would be
     * found to be satisfiable when it is in fact unsatisfiable.
     */
    val input = Set(
      SubObjectPropertyOfAxiom(S1, R),
      TransitiveObjectPropertyAxiom(S1),
      SubClassOfAxiom(X1, ObjectAllValuesFrom(R, C)),
      SubClassOfAxiom(X1, ObjectSomeValuesFrom(S1, ObjectSomeValuesFrom(S1, ObjectComplementOf(C))))
    )
    // Expected result: X1 is unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X1)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBug6 [from HermiT]") {
    val input = Set(
      SubObjectPropertyOfAxiom(S1, R),
      TransitiveObjectPropertyAxiom(S1),
      SubObjectPropertyOfAxiom(S2, R),
      TransitiveObjectPropertyAxiom(S2),
      SubObjectPropertyOfAxiom(P, S1),
      SubObjectPropertyOfAxiom(P, S2),
      EquivalentClassesAxiom(X1, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(P, ObjectSomeValuesFrom(S1, ObjectComplementOf(C))))),
      EquivalentClassesAxiom(X2, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(P, ObjectSomeValuesFrom(S2, ObjectComplementOf(C)))))
    )
    // Expected result: both X1 and X2 are unsatisfiable.
    val expected = Set(
      EquivalentClassesAxiom(Nothing, X1, X2)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Ian merge test 1 reduced [new]") {
    // If predicate symbols are larger than function symbols in the order (i.e., if B > A then B(x) > A(f(x))
    // then this test will show the performance bug.
    val input = Set(
      InverseObjectPropertiesAxiom(R, S),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(S, D))),
      EquivalentClassesAxiom(X, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C1),
                                                     ObjectSomeValuesFrom(R, C2),
                                                     ObjectAllValuesFrom(R, D)))
    )
    // Expected result: class X is satisfiable.
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

  test("testIanRecursiveDefinitionTest1 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, B), ObjectSomeValuesFrom(R1, B), ObjectSomeValuesFrom(R2, B),
                                              ObjectSomeValuesFrom(R3, B), ObjectSomeValuesFrom(R4, B), ObjectSomeValuesFrom(R5, B),
                                              ObjectSomeValuesFrom(R6, B), ObjectSomeValuesFrom(R7, B), ObjectSomeValuesFrom(R8, B),
                                              ObjectSomeValuesFrom(R9, B))),
      SubClassOfAxiom(B, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, A), ObjectSomeValuesFrom(R1, A), ObjectSomeValuesFrom(R2, A),
                                              ObjectSomeValuesFrom(R3, A), ObjectSomeValuesFrom(R4, A), ObjectSomeValuesFrom(R5, A),
                                              ObjectSomeValuesFrom(R6, A), ObjectSomeValuesFrom(R7, A), ObjectSomeValuesFrom(R8, A),
                                              ObjectSomeValuesFrom(R9, A)))
    )
    // Note that an empty set does not mean that the taxonomy is inconsistent, just that no non-trivial subsumptions
    // were derived. In particular, class A is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanRecursiveDefinitionTest2 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, B), ObjectSomeValuesFrom(R1, B), ObjectSomeValuesFrom(R2, B),
                                              ObjectSomeValuesFrom(R3, B), ObjectSomeValuesFrom(R4, B), ObjectSomeValuesFrom(R5, B),
                                              ObjectSomeValuesFrom(R6, B), ObjectSomeValuesFrom(R7, B), ObjectSomeValuesFrom(R8, B),
                                              ObjectSomeValuesFrom(R9, B))),
      SubClassOfAxiom(B, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, C), ObjectSomeValuesFrom(R1, C), ObjectSomeValuesFrom(R2, C),
                                              ObjectSomeValuesFrom(R3, C), ObjectSomeValuesFrom(R4, C), ObjectSomeValuesFrom(R5, C),
                                              ObjectSomeValuesFrom(R6, C), ObjectSomeValuesFrom(R7, C), ObjectSomeValuesFrom(R8, C),
                                              ObjectSomeValuesFrom(R9, C))),
      SubClassOfAxiom(C, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, A), ObjectSomeValuesFrom(R1, A), ObjectSomeValuesFrom(R2, A),
                                              ObjectSomeValuesFrom(R3, A), ObjectSomeValuesFrom(R4, A), ObjectSomeValuesFrom(R5, A),
                                              ObjectSomeValuesFrom(R6, A), ObjectSomeValuesFrom(R7, A), ObjectSomeValuesFrom(R8, A),
                                              ObjectSomeValuesFrom(R9, A)))
    )
    // Expected result: class A is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanRecursiveDefinitionTest3 [from HermiT]") {
    // This example creates a cycle in the context structure.
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, B), ObjectSomeValuesFrom(R1, B), ObjectSomeValuesFrom(R2, B),
                                              ObjectSomeValuesFrom(R3, B), ObjectSomeValuesFrom(R4, B), ObjectSomeValuesFrom(R5, B),
                                              ObjectSomeValuesFrom(R6, B), ObjectSomeValuesFrom(R7, B), ObjectSomeValuesFrom(R8, B),
                                              ObjectSomeValuesFrom(R9, B))),
      SubClassOfAxiom(B, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, C), ObjectSomeValuesFrom(R1, C), ObjectSomeValuesFrom(R2, C),
                                              ObjectSomeValuesFrom(R3, C), ObjectSomeValuesFrom(R4, C), ObjectSomeValuesFrom(R5, C),
                                              ObjectSomeValuesFrom(R6, C), ObjectSomeValuesFrom(R7, C), ObjectSomeValuesFrom(R8, C),
                                              ObjectSomeValuesFrom(R9, C))),
      SubClassOfAxiom(C, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, D), ObjectSomeValuesFrom(R1, D), ObjectSomeValuesFrom(R2, D),
                                              ObjectSomeValuesFrom(R3, D), ObjectSomeValuesFrom(R4, D), ObjectSomeValuesFrom(R5, D),
                                              ObjectSomeValuesFrom(R6, D), ObjectSomeValuesFrom(R7, D), ObjectSomeValuesFrom(R8, D),
                                              ObjectSomeValuesFrom(R9, D))),
      SubClassOfAxiom(D, ObjectIntersectionOf(ObjectSomeValuesFrom(R0, A), ObjectSomeValuesFrom(R1, A), ObjectSomeValuesFrom(R2, A),
                                              ObjectSomeValuesFrom(R3, A), ObjectSomeValuesFrom(R4, A), ObjectSomeValuesFrom(R5, A),
                                              ObjectSomeValuesFrom(R6, A), ObjectSomeValuesFrom(R7, A), ObjectSomeValuesFrom(R8, A),
                                              ObjectSomeValuesFrom(R9, A)))
    )
    // Expected result: class A is satisfiable.
    val expected = Set.empty[Axiom]
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBackjumping1 [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C1, ObjectIntersectionOf(ObjectUnionOf(A0, B0), ObjectUnionOf(A1, B1), ObjectUnionOf(A2, B2),
                                               ObjectUnionOf(A3, B3), ObjectUnionOf(A4, B4), ObjectUnionOf(A5, B5),
                                               ObjectUnionOf(A6, B6), ObjectUnionOf(A7, B7), ObjectUnionOf(A8, B8),
                                               ObjectUnionOf(A9, B9), ObjectUnionOf(A10, B10), ObjectUnionOf(A11, B11),
                                               ObjectUnionOf(A12, B12), ObjectUnionOf(A13, B13), ObjectUnionOf(A14, B14),
                                               ObjectUnionOf(A15, B15), ObjectUnionOf(A16, B16), ObjectUnionOf(A17, B17),
                                               ObjectUnionOf(A18, B18), ObjectUnionOf(A19, B19), ObjectUnionOf(A20, B20),
                                               ObjectUnionOf(A21, B21), ObjectUnionOf(A22, B22), ObjectUnionOf(A23, B23),
                                               ObjectUnionOf(A24, B24), ObjectUnionOf(A25, B25), ObjectUnionOf(A26, B26),
                                               ObjectUnionOf(A27, B27), ObjectUnionOf(A28, B28), ObjectUnionOf(A29, B29),
                                               ObjectUnionOf(A30, B30), ObjectUnionOf(A31, B31))),
      SubClassOfAxiom(C2, ObjectIntersectionOf(ObjectUnionOf(A, B), ObjectUnionOf(A, ObjectComplementOf(B)))),
      SubClassOfAxiom(C3, ObjectIntersectionOf(ObjectUnionOf(ObjectComplementOf(A), B), ObjectUnionOf(ObjectComplementOf(A), ObjectComplementOf(B)))),
      SubClassOfAxiom(C4, ObjectSomeValuesFrom(R, C2)),
      SubClassOfAxiom(C5, ObjectAllValuesFrom(R, C3)),
      SubClassOfAxiom(X, ObjectIntersectionOf(C1, C4, C5))
    )
    // Expected result: class X should be unsatisfiable.
    val expected = Set(
      SubClassOfAxiom(C2, A),
      EquivalentClassesAxiom(X, Nothing)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBackjumping2 [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C2, ObjectIntersectionOf(ObjectUnionOf(A, B), ObjectUnionOf(A, ObjectComplementOf(B)))),
      SubClassOfAxiom(C3, ObjectIntersectionOf(ObjectUnionOf(ObjectComplementOf(A), B), ObjectUnionOf(ObjectComplementOf(A), ObjectComplementOf(B)))),
      SubClassOfAxiom(C4, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C8))),
      SubClassOfAxiom(C5, ObjectAllValuesFrom(R, ObjectIntersectionOf(C3, C9))),
      SubClassOfAxiom(C6, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C10))),
      SubClassOfAxiom(C7, ObjectAllValuesFrom(R, ObjectIntersectionOf(C3, C11))),
      SubClassOfAxiom(X, ObjectIntersectionOf(ObjectUnionOf(A0, B0), ObjectUnionOf(A1, B1), ObjectUnionOf(A2, B2),
                                              ObjectUnionOf(A3, B3), ObjectUnionOf(A4, B4), ObjectUnionOf(A5, B5),
                                              ObjectUnionOf(A6, B6), ObjectUnionOf(A7, B7), ObjectUnionOf(A8, B8),
                                              ObjectUnionOf(A9, B9), ObjectUnionOf(A10, B10), ObjectUnionOf(A11, B11),
                                              ObjectUnionOf(A12, B12), ObjectUnionOf(A13, B13), ObjectUnionOf(A14, B14),
                                              ObjectUnionOf(A15, B15), ObjectUnionOf(A16, B16), ObjectUnionOf(A17, B17),
                                              ObjectUnionOf(A18, B18), ObjectUnionOf(A19, B19), ObjectUnionOf(A20, B20),
                                              ObjectUnionOf(A21, B21), ObjectUnionOf(A22, B22), ObjectUnionOf(A23, B23),
                                              ObjectUnionOf(A24, B24), ObjectUnionOf(A25, B25), ObjectUnionOf(A26, B26),
                                              ObjectUnionOf(A27, B27), ObjectUnionOf(A28, B28), ObjectUnionOf(A29, B29),
                                              ObjectUnionOf(A30, B30), ObjectUnionOf(A31, B31)))
    )
    // Expected result: class X should be satisfiable.
    val expected = Set(
      SubClassOfAxiom(C2, A)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("testIanBackjumping3 [from HermiT]") {
    val input = Set(
      SubClassOfAxiom(C2, ObjectIntersectionOf(ObjectUnionOf(A, B), ObjectUnionOf(A, ObjectComplementOf(B)))),
      SubClassOfAxiom(C3, ObjectIntersectionOf(ObjectUnionOf(ObjectComplementOf(A), B), ObjectUnionOf(ObjectComplementOf(A), ObjectComplementOf(B)))),
      SubClassOfAxiom(C4, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C8))),
      SubClassOfAxiom(C5, ObjectAllValuesFrom(R, ObjectIntersectionOf(C3, C9))),
      SubClassOfAxiom(C6, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C2, C10))),
      SubClassOfAxiom(C7, ObjectAllValuesFrom(R, ObjectIntersectionOf(C3, C11))),
      SubClassOfAxiom(X, ObjectIntersectionOf(ObjectUnionOf(A0, B0),  ObjectUnionOf(A1, B1), ObjectUnionOf(A2, B2),
                                              ObjectUnionOf(A3, B3), ObjectUnionOf(A4, B4), ObjectUnionOf(A5, B5),
                                              ObjectUnionOf(A6, B6), ObjectUnionOf(A7, B7), ObjectUnionOf(A8, B8),
                                              ObjectUnionOf(A9, B9), ObjectUnionOf(A10, B10), ObjectUnionOf(A11, B11),
                                              ObjectUnionOf(A12, B12), ObjectUnionOf(A13, B13), ObjectUnionOf(A14, B14),
                                              ObjectUnionOf(A15, B15), ObjectUnionOf(A16, B16), ObjectUnionOf(A17, B17),
                                              ObjectUnionOf(A18, B18), ObjectUnionOf(A19, B19), ObjectUnionOf(A20, B20),
                                              ObjectUnionOf(A21, B21), ObjectUnionOf(A22, B22), ObjectUnionOf(A23, B23),
                                              ObjectUnionOf(A24, B24), ObjectUnionOf(A25, B25), ObjectUnionOf(A26, B26),
                                              ObjectUnionOf(A27, B27), ObjectUnionOf(A28, B28), ObjectUnionOf(A29, B29),
                                              ObjectUnionOf(A30, B30), ObjectUnionOf(A31, B31), ObjectUnionOf(C4, C6), ObjectUnionOf(C5, C7)))
    )
    // Expected result: class X should be unsatisfiable.
    val expected = Set(
      SubClassOfAxiom(C2, A),
      EquivalentClassesAxiom(X, Nothing)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Oxford ontology 00775 bug 1") {
    val Father = OWLClass(IRI(p, "Father"))
    val Forefather = OWLClass(IRI(p, "Forefather"))
    val Person = OWLClass(IRI(p, "Person"))
    val Man = OWLClass(IRI(p, "Man"))
    val isFatherOf = ObjectProperty(IRI(p, "isFatherOf"))
    val isForefatherOf = ObjectProperty(IRI(p, "isForefatherOf"))
    val hasFather = ObjectProperty(IRI(p, "hasFather"))
    val hasForeFather = ObjectProperty(IRI(p, "hasForeFather"))
    val input = Set(
      EquivalentClassesAxiom(Father, ObjectIntersectionOf(ObjectSomeValuesFrom(isFatherOf, Person), Man)),
      EquivalentClassesAxiom(Forefather, ObjectIntersectionOf(ObjectSomeValuesFrom(isForefatherOf, Person), Man)),
      InverseObjectPropertiesAxiom(hasFather, isFatherOf),
      InverseObjectPropertiesAxiom(hasForeFather, isForefatherOf),
      SubObjectPropertyOfAxiom(hasFather, hasForeFather),
      TransitiveObjectPropertyAxiom(hasForeFather)
    )
    // Expected result: class Father is a Forefather, and Forefather is a Man.
    val expected = Set(
      SubClassOfAxiom(Father, Forefather),
      SubClassOfAxiom(Forefather, Man)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Oxford ontology 00775 bug 2") {
    val Forefather = OWLClass(IRI(p, "Forefather"))
    val Person = OWLClass(IRI(p, "Person"))
    val Man = OWLClass(IRI(p, "Man"))
    val MaleAncestor = OWLClass(IRI(p, "MaleAncestor"))
    val isForefatherOf = ObjectProperty(IRI(p, "isForefatherOf"))
    val isAncestorOf = ObjectProperty(IRI(p, "isAncestorOf"))
    val input = Set(
      EquivalentClassesAxiom(Forefather, ObjectIntersectionOf(ObjectSomeValuesFrom(isForefatherOf, Person), Man)),
      EquivalentClassesAxiom(MaleAncestor, ObjectIntersectionOf(ObjectSomeValuesFrom(isAncestorOf, Person), Man)),
      SubObjectPropertyOfAxiom(isForefatherOf, isAncestorOf),
      TransitiveObjectPropertyAxiom(isForefatherOf)
    )
    // Expected result: class Forefather is a MaleAncestor, and MaleAncestor is a Man.
    val expected = Set(
      SubClassOfAxiom(MaleAncestor, Man),
      SubClassOfAxiom(Forefather, MaleAncestor)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Complex RBox") {
    val nA = OWLClass(IRI("http://www.example.com/test#A"))
    val nB = OWLClass(IRI("http://www.example.com/test#B"))
    val nR = ObjectProperty(IRI("http://www.example.com/test#R"))
    val nS = ObjectProperty(IRI("http://www.example.com/test#S"))
    val nT = ObjectProperty(IRI("http://www.example.com/test#T"))
    val nQ = ObjectProperty(IRI("http://www.example.com/test#Q"))
    val input = Set(
      SubClassOfAxiom(nA, ObjectSomeValuesFrom(nR, Thing)),
      SubClassOfAxiom(ObjectSomeValuesFrom(nS, Thing), nB),
      InverseObjectPropertiesAxiom(nT, nS),
      InverseObjectPropertiesAxiom(nQ, nR),
      SubObjectPropertyOfAxiom(nQ, nT),
      SubObjectPropertyOfAxiom(nR, nS)
    )
    // Expected result: class A is a B.
    val expected = Set(
      SubClassOfAxiom(nA, nB)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Disjunctions performance") {
    /* Tests the performance of disjunctions. The reasoner should not push back conclusions from result from the fillers
     * of existentials from the successor contexts to the predecessor contexts in this example. If the reasoner attempts
     * to, then the context that corresponds to A will generate a large number of clauses, most of which will be
     * eliminated by the Elim rule.
     */
    val input = Set(
      EquivalentClassesAxiom(A, ObjectUnionOf(ObjectIntersectionOf(ObjectSomeValuesFrom(S, X1),
                                                                   ObjectSomeValuesFrom(T, Y1)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X2),
                                                                   ObjectSomeValuesFrom(T, Y2)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X3),
                                                                   ObjectSomeValuesFrom(T, Y3)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X4),
                                                                   ObjectSomeValuesFrom(T, Y4))))
    )
    // Expected result: class A is not a subclass of anything.
    val expected = Set.empty
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Bad performance bug") {
    val input = Set(
      EquivalentClassesAxiom(A, ObjectIntersectionOf(B,
                                                     ObjectSomeValuesFrom(R1, C),
                                                     ObjectSomeValuesFrom(R2, D),
                                                     ObjectSomeValuesFrom(R3, B),
                                                     ObjectSomeValuesFrom(R4, E),
                                                     ObjectSomeValuesFrom(T, X1),
                                                     ObjectSomeValuesFrom(T, X2),
                                                     ObjectSomeValuesFrom(T, X3),
                                                     ObjectSomeValuesFrom(T, X4),
                                                     ObjectSomeValuesFrom(T, X5),
                                                     ObjectSomeValuesFrom(T, X6),
                                                     ObjectSomeValuesFrom(T, X7),
                                                     ObjectSomeValuesFrom(T, X8),
                                                     ObjectSomeValuesFrom(T, X9),
                                                     ObjectSomeValuesFrom(S, Y1),
                                                     ObjectSomeValuesFrom(S, Y2),
                                                     ObjectSomeValuesFrom(S, Y3),
                                                     ObjectSomeValuesFrom(S, Y4)))
    )
    // Expected result: class A is a B.
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

  test("Bad performance bug 2 simplified"){
    val input = Set(
      SubClassOfAxiom(A, ObjectUnionOf(ObjectIntersectionOf(ObjectSomeValuesFrom(S, X1),
                                                                   ObjectSomeValuesFrom(T, Y1)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X2),
                                                                   ObjectSomeValuesFrom(T, Y2)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X3),
                                                                   ObjectSomeValuesFrom(T, Y3)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X4),
                                                                   ObjectSomeValuesFrom(T, Y4)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X5),
                                                                   ObjectSomeValuesFrom(T, Y5)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X6),
                                                                   ObjectSomeValuesFrom(T, Y6)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X7),
                                                                   ObjectSomeValuesFrom(T, Y7)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X8),
                                                                   ObjectSomeValuesFrom(T, Y8)),
                                              ObjectIntersectionOf(ObjectSomeValuesFrom(S, X9),
                                                                   ObjectSomeValuesFrom(T, Y9)))),
      SubClassOfAxiom(ObjectUnionOf(ObjectIntersectionOf(ObjectSomeValuesFrom(S, X1),
                                                         ObjectSomeValuesFrom(T, Y1)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X2),
                                                         ObjectSomeValuesFrom(T, Y2)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X3),
                                                         ObjectSomeValuesFrom(T, Y3)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X4),
                                                         ObjectSomeValuesFrom(T, Y4)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X5),
                                                         ObjectSomeValuesFrom(T, Y5)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X6),
                                                         ObjectSomeValuesFrom(T, Y6)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X7),
                                                         ObjectSomeValuesFrom(T, Y7)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X8),
                                                         ObjectSomeValuesFrom(T, Y8)),
                                    ObjectIntersectionOf(ObjectSomeValuesFrom(S, X9),
                                                         ObjectSomeValuesFrom(T, Y9))), A)
    )
    // Expected result: class A is owl:Thing.
    val expected = Set.empty
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Bad performance bug 2"){
    val input = Set(
      EquivalentClassesAxiom(A, ObjectIntersectionOf(B,
                                                     ObjectUnionOf(ObjectIntersectionOf(ObjectSomeValuesFrom(S, X1),
                                                                                        ObjectSomeValuesFrom(T, Y1)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X2),
                                                                                        ObjectSomeValuesFrom(T, Y2)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X3),
                                                                                        ObjectSomeValuesFrom(T, Y3)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X4),
                                                                                        ObjectSomeValuesFrom(T, Y4)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X5),
                                                                                        ObjectSomeValuesFrom(T, Y5)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X6),
                                                                                        ObjectSomeValuesFrom(T, Y6)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X7),
                                                                                        ObjectSomeValuesFrom(T, Y7)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X8),
                                                                                        ObjectSomeValuesFrom(T, Y8)),
                                                                   ObjectIntersectionOf(ObjectSomeValuesFrom(S, X9),
                                                                                        ObjectSomeValuesFrom(T, Y9))),
                                                     ObjectSomeValuesFrom(R1, A1),
                                                     ObjectSomeValuesFrom(R2, A2),
                                                     ObjectSomeValuesFrom(R2, A3),
                                                     ObjectSomeValuesFrom(R3, A4),
                                                     ObjectSomeValuesFrom(R4, A5),
                                                     ObjectSomeValuesFrom(R4, A6),
                                                     ObjectSomeValuesFrom(R4, A7),
                                                     ObjectSomeValuesFrom(R4, A8),
                                                     ObjectSomeValuesFrom(R4, A9),
                                                     ObjectSomeValuesFrom(R4, A10),
                                                     ObjectSomeValuesFrom(R4, A11),
                                                     ObjectSomeValuesFrom(R4, A12),
                                                     ObjectSomeValuesFrom(R4, A13),
                                                     ObjectSomeValuesFrom(R5, A14),
                                                     ObjectSomeValuesFrom(R5, A15),
                                                     ObjectSomeValuesFrom(R5, A16),
                                                     ObjectSomeValuesFrom(R6, A17),
                                                     ObjectSomeValuesFrom(R6, A18),
                                                     ObjectSomeValuesFrom(R6, A19)))
    )
    // Expected result: class A is a B.
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

  test("00778 bug 1") {
    val input = Set(
      SubClassOfAxiom(C1, ObjectIntersectionOf(C2, ObjectSomeValuesFrom(R, C3))),
      SubObjectPropertyOfAxiom(R, S),
      EquivalentClassesAxiom(C2, ObjectIntersectionOf(C4, ObjectUnionOf(C1, ObjectSomeValuesFrom(S, C1)))),
      EquivalentClassesAxiom(C5, ObjectIntersectionOf(C4, ObjectUnionOf(C3, ObjectSomeValuesFrom(S, C3))))
    )
    // Expected result: class C1 is a subclass of C5.
    val expected = Set(
      SubClassOfAxiom(C1, C2),
      SubClassOfAxiom(C1, C5),
      SubClassOfAxiom(C2, C4),
      SubClassOfAxiom(C5, C4),
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("ordering test 1") {
    val Giraffe = OWLClass(IRI("http://www.example.com/test#Giraffe"))
    val Leaf = OWLClass(IRI("http://www.example.com/test#Leaf"))
    val Plant = OWLClass(IRI("http://www.example.com/test#Plant"))
    val Animal = OWLClass(IRI("http://www.example.com/test#Animal"))
    val Vegetarian = OWLClass(IRI("http://www.example.com/test#Vegetarian"))
    val eats = ObjectProperty(IRI("http://www.example.com/test#eats"))
    val partOf = ObjectProperty(IRI("http://www.example.com/test#partOf"))
    val input = Set(
      SubClassOfAxiom(Giraffe, ObjectAllValuesFrom(eats, Leaf)),
      SubClassOfAxiom(Leaf, ObjectSomeValuesFrom(partOf, Plant)),
      SubClassOfAxiom(Giraffe, Animal),
      DisjointClassesAxiom(Animal, ObjectSomeValuesFrom(partOf, Plant)),
      DisjointClassesAxiom(ObjectSomeValuesFrom(partOf, Animal), ObjectSomeValuesFrom(partOf, Plant)),
      SubClassOfAxiom(ObjectIntersectionOf(Animal, ObjectAllValuesFrom(eats, ObjectComplementOf(Animal))), Vegetarian)
    )
    /* This input will be structurally transformed to the following:
     * [[
     * all0(x) AND all1(x) ->
     * all0(x) AND Animal(x) ->
     * Animal(x) -> disjunct0(x) OR Vegetarian(x)
     * disjunct0(x) -> Animal(f2(x))
     * disjunct0(x) -> eats(x, f2(x))
     * Giraffe(x) -> Animal(x)
     * Giraffe(x) AND eats(x, z3) -> Leaf(z3)
     * Leaf(x) -> partOf(x, f1(x))
     * Leaf(x) -> Plant(f1(x))
     * partOf(z1, x) AND Animal(x) -> all1(z1)
     * partOf(z1, x) AND Plant(x) -> all0(z1)
     * ]]
     * If the ordering definition does not correctly ensure that !(disjunct0(x) < Vegetarian(x)), then the calculus will
     * be incomplete.
     */
    // Expected result: class Giraffe is a subclass of Vegetarian.
    val expected = Set(
      SubClassOfAxiom(Giraffe, Animal),
      SubClassOfAxiom(Giraffe, Vegetarian)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("incomparable query concepts") {
    // This interactions happen in the 00001 ontology from the Oxford ontology repository.
    // This example demonstrates why the query concepts to be incomparable.
    val N0 = OWLClass(IRI("http://www.example.com/test#N0"))
    val N1 = OWLClass(IRI("http://www.example.com/test#N1"))
    val N2 = OWLClass(IRI("http://www.example.com/test#N2"))
    val N3 = OWLClass(IRI("http://www.example.com/test#N3"))
    val NClass = OWLClass(IRI("http://www.example.com/test#NClass"))
    val Status = OWLClass(IRI("http://www.example.com/test#Status"))
    val StatusNegative = OWLClass(IRI("http://www.example.com/test#StatusNegative"))
    val StatusPositive = OWLClass(IRI("http://www.example.com/test#StatusPositive"))
    val input = Set(
      SubClassOfAxiom(N0, NClass),
      SubClassOfAxiom(N1, NClass),
      SubClassOfAxiom(N2, NClass),
      SubClassOfAxiom(N3, NClass),
      SubClassOfAxiom(Status, ObjectUnionOf(StatusPositive, StatusNegative)),
      SubClassOfAxiom(StatusNegative, N0),
      SubClassOfAxiom(StatusPositive, ObjectUnionOf(N1, N2, N3))
    )
    val expected = Set(
      SubClassOfAxiom(N0, NClass),
      SubClassOfAxiom(N1, NClass),
      SubClassOfAxiom(N2, NClass),
      SubClassOfAxiom(N3, NClass),
      SubClassOfAxiom(Status, NClass),
      SubClassOfAxiom(StatusNegative, N0),
      SubClassOfAxiom(StatusPositive, NClass)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("taxonomy bug") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(ObjectComplementOf(A), C),
      SubClassOfAxiom(D, B)
    )
    // Expected result: class B is equivalent to owl:Thing.
    val expected = Set(
      EquivalentClassesAxiom(B, Thing),
      SubClassOfAxiom(C, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("pred rule bug (reduced from ianT13)") {
    // This bug was sensitive to the naming of IRIs.
    val a1 = OWLClass(IRI("http://www.example.com/test#a1"))
    val a3a = OWLClass(IRI("http://www.example.com/test#a3a"))
    val a3b = OWLClass(IRI("http://www.example.com/test#a3b"))
    val input = Set(
      SubClassOfAxiom(X1, a1),
      SubClassOfAxiom(X1, a3a),
      SubClassOfAxiom(a3a, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(ObjectInverseOf(R), ObjectSomeValuesFrom(R, D)))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, D), a3b),
      SubClassOfAxiom(ObjectIntersectionOf(a3b, a1), X2),
      SubClassOfAxiom(a3b, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectIntersectionOf(a3a, ObjectComplementOf(a3b)), X6)
    )
    // Expected result: class X1 is a subclass of X2.
    val expected = Set(
      SubClassOfAxiom(X1, X2),
      SubClassOfAxiom(X1, a1),
      SubClassOfAxiom(X1, a3a),
      SubClassOfAxiom(a3a, a3b)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("pizza bug") {
    val input = Set(
      SubClassOfAxiom(A, ObjectAllValuesFrom(R, ObjectUnionOf(C1, C2))),
      SubClassOfAxiom(C1, C2),
      SubClassOfAxiom(C2, X1),
      DisjointClassesAxiom(X2, X1),
      SubClassOfAxiom(ObjectAllValuesFrom(R, ObjectComplementOf(X2)), B)
    )
    // Expected result: class A is a subclass of B.
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(C1, C2),
      SubClassOfAxiom(C2, X1)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

}
