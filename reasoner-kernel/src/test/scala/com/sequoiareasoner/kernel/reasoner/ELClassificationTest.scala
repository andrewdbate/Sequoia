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

/** Tests to check that the computed taxonomy is both correct and complete for EL ontologies.
  *
  * Some test cases are adapted from the ELK and Pellet reasoners.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class ELClassificationTest extends FunSuite with RandomTestOrder {
  import ClassificationTestUtils._
  import OWLAxiomBuilder._
  import CommonNames._

  // TODO: In all cases also query the reasoner for expressivity, and assert that it is EL.
  // TODO: only classify concepts on the LHS

  // TODO: For the "disjointSelf [from ELK]" test case, add an example testing satisfiability with the following assertions:
  // "A unsatisfiable", "B satisfiable", "C unsatisfiable".
  // Similarity for "disjoint [from ELK]" check the assertions:
  // "A unsatisfiable", "B unsatisfiable", "C satisfiable"

  test("ancestors [from ELK]") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(C, D)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(C, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("bottom [from ELK]") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, Nothing),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, A))
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, B, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("ancestors and bottom [new]") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(B, Nothing),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, A))
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, B, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("conjunctions [from ELK]") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(ObjectIntersectionOf(B, C, D), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("disjoint [from ELK]") {
    val input = Set(
      DisjointClassesAxiom(A, B, C),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(B, C)
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("disjointSelf [from ELK]") {
    val input = Set(
      DisjointClassesAxiom(A, B, A, A),
      DisjointClassesAxiom(C, C)
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("simple equivalent [from ELK]") {
    val input = Set(
      EquivalentClassesAxiom(B, C)
    )
    val expected = Set(
      EquivalentClassesAxiom(B, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("existentials simplified [adapted from ELK]") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), D)
    )
    val expected = Set(
      SubClassOfAxiom(A, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("existentials [from ELK]") {
    val input = Set(
      EquivalentClassesAxiom(B, C),
      ObjectPropertyDomainAxiom(S, E),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, C), D),
      SubObjectPropertyOfAxiom(R, S)
    )
    val expected = Set(
      EquivalentClassesAxiom(B, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("forest simplified [adapted from ELK]") {
    val input = Set(
      SubClassOfAxiom(A, X),
      SubClassOfAxiom(B, X),
      SubClassOfAxiom(C, X),
      SubClassOfAxiom(D, X),
      SubClassOfAxiom(E, X),
      SubClassOfAxiom(F, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(input), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("forest [from ELK]") {
    val black = OWLClass(IRI(p, "black"))
    val blue = OWLClass(IRI(p, "blue"))
    val brown = OWLClass(IRI(p, "brown"))
    val brownThing = OWLClass(IRI(p, "brown-thing"))
    val color = OWLClass(IRI(p, "color"))
    val forest = OWLClass(IRI(p, "forest"))
    val green = OWLClass(IRI(p, "green"))
    val greenThing = OWLClass(IRI(p, "green-thing"))
    val leaf = OWLClass(IRI(p, "leaf"))
    val red = OWLClass(IRI(p, "red"))
    val tree = OWLClass(IRI(p, "tree"))
    val trunk = OWLClass(IRI(p, "trunk"))
    val yellow = OWLClass(IRI(p, "yellow"))
    val hasColor = ObjectProperty(IRI(p, "has-color"))
    val hasPart = ObjectProperty(IRI(p, "has-part"))
    val input = Set(
      SubClassOfAxiom(black, color),
      SubClassOfAxiom(blue, color),
      SubClassOfAxiom(brown, color),
      SubClassOfAxiom(forest, ObjectSomeValuesFrom(hasPart, tree)),
      SubClassOfAxiom(green, color),
      SubClassOfAxiom(leaf, ObjectSomeValuesFrom(hasColor, green)),
      SubClassOfAxiom(ObjectSomeValuesFrom(hasColor, brown), brownThing),
      SubClassOfAxiom(ObjectSomeValuesFrom(hasColor, green), greenThing),
      SubClassOfAxiom(red, color),
      SubClassOfAxiom(tree, ObjectIntersectionOf(ObjectSomeValuesFrom(hasPart, trunk), ObjectSomeValuesFrom(hasPart, leaf))),
      SubClassOfAxiom(trunk, ObjectSomeValuesFrom(hasColor, brown)),
      SubClassOfAxiom(yellow, color),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(hasPart, hasColor), hasColor),
      TransitiveObjectPropertyAxiom(hasPart)
    )
    val expected = Set(
      SubClassOfAxiom(black, color),
      SubClassOfAxiom(blue, color),
      SubClassOfAxiom(brown, color),
      SubClassOfAxiom(forest, brownThing),
      SubClassOfAxiom(forest, greenThing),
      SubClassOfAxiom(green, color),
      SubClassOfAxiom(leaf, greenThing),
      SubClassOfAxiom(red, color),
      SubClassOfAxiom(tree, brownThing),
      SubClassOfAxiom(tree, greenThing),
      SubClassOfAxiom(trunk, brownThing),
      SubClassOfAxiom(yellow, color)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("inconsistent [from ELK]") {
    // This example results in a context structure that contains a cycle.
    val input = Set(
      EquivalentClassesAxiom(A, C),
      ObjectPropertyDomainAxiom(T, Nothing),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(T, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, B), A),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, B)),
      SubObjectPropertyOfAxiom(R, S)
    )
    computeTaxonomy(input) match {
      case None =>
        // This is the correct result
      case Some(actual) =>
        fail(s"Ontology should have been inconsistent!")
    }
  }

  test("kangaroo [from ELK]") {
    val Animal = OWLClass(IRI(p, "Animal"))
    val Beast = OWLClass(IRI(p, "Beast"))
    val Human = OWLClass(IRI(p, "Human"))
    val Irrational = OWLClass(IRI(p, "Irrational"))
    val Kangaroo = OWLClass(IRI(p, "Kangaroo"))
    val KangarooInfant = OWLClass(IRI(p, "KangarooInfant"))
    val MaternityKangaroo = OWLClass(IRI(p, "MaternityKangaroo"))
    val Parent = OWLClass(IRI(p, "Parent"))
    val Pouch = OWLClass(IRI(p, "Pouch"))
    val Rational = OWLClass(IRI(p, "Rational"))
    val hasBodyPart = ObjectProperty(IRI(p, "has-body-part"))
    val hasChild = ObjectProperty(IRI(p, "has-child"))
    val livesIn = ObjectProperty(IRI(p, "lives-in"))
    val input = Set(
      DisjointClassesAxiom(Irrational, Rational),
      DisjointClassesAxiom(Rational, Irrational),
      EquivalentClassesAxiom(MaternityKangaroo, ObjectIntersectionOf(Kangaroo, ObjectSomeValuesFrom(hasBodyPart, Pouch), ObjectSomeValuesFrom(hasChild, KangarooInfant))),
      EquivalentClassesAxiom(Parent, ObjectSomeValuesFrom(hasChild, Thing)),
      ObjectPropertyDomainAxiom(hasChild, Human),
      ObjectPropertyRangeAxiom(hasChild, Human),
      SubClassOfAxiom(Beast, ObjectIntersectionOf(Irrational, Animal)),
      SubClassOfAxiom(Human, ObjectIntersectionOf(Rational, Animal)),
      SubClassOfAxiom(Kangaroo, Beast),
      SubClassOfAxiom(KangarooInfant, ObjectIntersectionOf(ObjectSomeValuesFrom(livesIn, Pouch), Kangaroo))
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, MaternityKangaroo),
      SubClassOfAxiom(Beast, Animal),
      SubClassOfAxiom(Beast, Irrational),
      SubClassOfAxiom(Human, Animal),
      SubClassOfAxiom(Human, Rational),
      SubClassOfAxiom(Kangaroo, Beast),
      SubClassOfAxiom(KangarooInfant, Kangaroo),
      SubClassOfAxiom(Parent, Human)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("propertyChains [from ELK]") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R1, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R2, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R3, D)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(R4, E)),
      SubClassOfAxiom(ObjectIntersectionOf(Thing, ObjectSomeValuesFrom(T, Thing)), X),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2, S), T),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R3, R4), S)
    )
    val expected = Set(
      SubClassOfAxiom(A, X)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  ignore("propertyChainsWithInverses [from ELK] (not a regular RBox)") {
    // This RBox is not regular according to Horrocks and Sattler.
    val input = Set(
      EquivalentClassesAxiom(A, ObjectSomeValuesFrom(T, A)),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(ObjectInverseOf(T), T, T), T)
    )
    val expected = Set(DeclarationAxiom(A))
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("simple reflexive role") {
    val input = Set(
      ReflexiveObjectPropertyAxiom(R),
      EquivalentClassesAxiom(B, ObjectSomeValuesFrom(R, A)),
      EquivalentClassesAxiom(D, ObjectSomeValuesFrom(S, C)),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), S),
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(C, D),
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("reflexiveRole [from ELK]") {
    // S is implicitly reflexive since R and H1 are.
    // Tests that :H (an implicitly reflexive role) is properly handled, i.e., :T1 o :T2 -> U.
    // Tests that :R is properly eliminated from these chains to still entail V1 -> V3 and, thus, K -> L.
    val input = Set(
      EquivalentClassesAxiom(B, ObjectSomeValuesFrom(R, A)),
      EquivalentClassesAxiom(D, ObjectSomeValuesFrom(S, C)),
      EquivalentClassesAxiom(K, ObjectSomeValuesFrom(V1, K1)),
      EquivalentClassesAxiom(L, ObjectSomeValuesFrom(V3, K1)),
      ReflexiveObjectPropertyAxiom(H),
      ReflexiveObjectPropertyAxiom(R),
      SubClassOfAxiom(C1, ObjectSomeValuesFrom(T, ObjectSomeValuesFrom(Q, C2))),
      SubClassOfAxiom(ObjectSomeValuesFrom(U, C2), F),
      SubObjectPropertyOfAxiom(H, H1),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, H1), S),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, V1), V2),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(T, Q, H), U),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(V2, R), V3)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C1, F),
      SubClassOfAxiom(K, L)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("top [from ELK]") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(Thing, C)
    )
    val expected = Set(
      EquivalentClassesAxiom(Thing, C),
      SubClassOfAxiom(A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("test existentials"){
    val input = Set(
      EquivalentClassesAxiom(B, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, C), D),
      SubObjectPropertyOfAxiom(R, S)
    )
    val expected = Set(
      EquivalentClassesAxiom(B, C),
      SubClassOfAxiom(A, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("test conjunctions"){
    val input = Set(
      EquivalentClassesAxiom(X, ObjectIntersectionOf(B, C)),
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(ObjectIntersectionOf(B, C), D)
    )
    val expected = Set(
      SubClassOfAxiom(A, X),
      SubClassOfAxiom(X, B),
      SubClassOfAxiom(X, C),
      SubClassOfAxiom(X, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL1 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, ObjectSomeValuesFrom(R, C))),
      SubClassOfAxiom(ObjectIntersectionOf(B, D), E),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, Thing), D)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL2 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, D), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, E),
      SubClassOfAxiom(C, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL3a [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C, E),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(D, E)), F)
    )
    val expected = Set(
      SubClassOfAxiom(A, F),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL3b [from Pellet]"){
    val input = Set(
      EquivalentClassesAxiom(G, ObjectIntersectionOf(D, E)),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C, E),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, G), F)
    )
    val expected = Set(
      SubClassOfAxiom(A, F),
      SubClassOfAxiom(C, G),
      SubClassOfAxiom(G, D),
      SubClassOfAxiom(G, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL3c [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, D))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, C), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL4 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, C, D)),
      SubClassOfAxiom(ObjectIntersectionOf(C, D), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL5a [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, F),
      SubClassOfAxiom(F, ObjectSomeValuesFrom(R, G)),
      SubClassOfAxiom(G, C),
      SubClassOfAxiom(G, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectSomeValuesFrom(R, B), C))), D)
    )
    val expected = Set(
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, D),
      SubClassOfAxiom(E, F),
      SubClassOfAxiom(G, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL5b [from Pellet]"){
    val input = Set(
      EquivalentClassesAxiom(X1, ObjectSomeValuesFrom(R, X2)),
      EquivalentClassesAxiom(X2, ObjectIntersectionOf(X3, C)),
      EquivalentClassesAxiom(X3, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, F),
      SubClassOfAxiom(F, ObjectSomeValuesFrom(R, G)),
      SubClassOfAxiom(G, C),
      SubClassOfAxiom(G, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectIntersectionOf(A, X1), D)
    )
    val expected = Set(
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, D),
      SubClassOfAxiom(E, F),
      SubClassOfAxiom(F, X1),
      SubClassOfAxiom(G, X2),
      SubClassOfAxiom(X2, C),
      SubClassOfAxiom(X2, X3)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL6 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, ObjectSomeValuesFrom(R, G)),
      SubClassOfAxiom(G, B),
      SubClassOfAxiom(G, C),
      SubClassOfAxiom(ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C))), D)
    )
    val expected = Set(
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, D),
      SubClassOfAxiom(G, B),
      SubClassOfAxiom(G, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL7 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(ObjectIntersectionOf(A, B), ObjectIntersectionOf(C, D, Thing)),
      SubClassOfAxiom(ObjectIntersectionOf(A, C), E),
      SubClassOfAxiom(ObjectIntersectionOf(A, D, Thing), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("EL8 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, C),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(D, Thing)), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, E),
      SubClassOfAxiom(B, C),
      SubClassOfAxiom(C, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("ELNormalization1 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, B)), D)
    )
    val expected = Set(
      SubClassOfAxiom(A, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("ELNormalization2 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, ObjectIntersectionOf(C, D))),
      SubClassOfAxiom(ObjectIntersectionOf(C, ObjectIntersectionOf(B, D)), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("ELNormalization3 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, ObjectIntersectionOf(C, D, ObjectIntersectionOf(E, F)))),
      SubClassOfAxiom(ObjectIntersectionOf(ObjectIntersectionOf(C, F), ObjectIntersectionOf(B, D, E)), G)
    )
    val expected = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, E),
      SubClassOfAxiom(A, F),
      SubClassOfAxiom(A, G)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Bottom1 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, Nothing),
      SubClassOfAxiom(C, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(Nothing, D)
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Bottom2 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(C, Nothing),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), C)
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Top1 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), D),
      SubClassOfAxiom(Thing, A)
    )
    val expected = Set(
      EquivalentClassesAxiom(Thing, A),
      SubClassOfAxiom(C, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("BottomWithObjectSomeValuesFrom1 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, Nothing)
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("BottomWithObjectSomeValuesFrom2 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, Nothing),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, A)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, B))
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, B, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Disjoint [from Pellet]"){
    val input = Set(
      DisjointClassesAxiom(C, D),
      EquivalentClassesAxiom(C, D),
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(ObjectIntersectionOf(A, B), Nothing)
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A, C, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("DisjointWithObjectSomeValuesFrom1 simplified [adapted from Pellet]"){
    val input = Set(
      DisjointClassesAxiom(ObjectSomeValuesFrom(R, C), D),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), ObjectSomeValuesFrom(R, C))
    )
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

  test("DisjointWithObjectSomeValuesFrom1 [from Pellet]"){
    val input = Set(
      DisjointClassesAxiom(ObjectSomeValuesFrom(S, C), D),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), ObjectSomeValuesFrom(R, C)),
      SubObjectPropertyOfAxiom(R, S)
    )
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

  test("DisjointWithObjectSomeValuesFrom2 [from Pellet]"){
    val input = Set(
      DisjointClassesAxiom(B, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C)))
    )
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

  test("Roles1a [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, B), C),
      SubObjectPropertyOfAxiom(R, S)
    )
    val expected = Set(
      SubClassOfAxiom(A, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles1b [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(D, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(ObjectIntersectionOf(D, ObjectSomeValuesFrom(S, B)), C),
      SubObjectPropertyOfAxiom(R, S)
    )
    val expected = Set(
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

  test("Roles2a simplified 1 [modified from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(Thing, ObjectUnionOf(C, ObjectAllValuesFrom(R, ObjectComplementOf(B)))),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R)
    )
    val expected = Set(
      SubClassOfAxiom(A, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles2a simplified 2 [modified from Pellet]"){
    // Test case is the same as `Roles2a simplified 1` but with the universal pushed to the LHS.
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), C),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R)
    )
    val expected = Set(
      SubClassOfAxiom(A, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles2a [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), C),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R),
      SubObjectPropertyOfAxiom(S, R)
    )
    val expected = Set(
      SubClassOfAxiom(A, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles2b [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(F, ObjectSomeValuesFrom(P, B))),
      SubClassOfAxiom(B, ObjectIntersectionOf(G, ObjectSomeValuesFrom(R, C))),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(P, D), E),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(R, S)
    )
    val expected = Set(
      SubClassOfAxiom(A, E),
      SubClassOfAxiom(A, F),
      SubClassOfAxiom(B, G)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles2c [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(P, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(S, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, D), E),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), R)
    )
    val expected = Set(
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles3a [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(P, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(S, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(P, D), E),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S, R), P)
    )
    val expected = Set(
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles3b [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(P, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(T, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(T, E)),
      SubClassOfAxiom(E, ObjectSomeValuesFrom(S, F)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, F), G),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, T, R), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, T, S), S)
    )
    val expected = Set(
      SubClassOfAxiom(A, G),
      SubClassOfAxiom(E, G)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Roles4 [from Pellet]"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(X0, ObjectSomeValuesFrom(P, B))),
      SubClassOfAxiom(B, ObjectIntersectionOf(X1, ObjectSomeValuesFrom(T, C))),
      SubClassOfAxiom(C, ObjectIntersectionOf(X2, ObjectSomeValuesFrom(R, D))),
      SubClassOfAxiom(D, ObjectIntersectionOf(X1, ObjectSomeValuesFrom(S, E))),
      SubClassOfAxiom(ObjectIntersectionOf(X0, ObjectSomeValuesFrom(P, E)), F),
      SubClassOfAxiom(X0, X1),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, T, R, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(T, R), S)
    )
    val expected = Set(
      SubClassOfAxiom(A, F),
      SubClassOfAxiom(A, X0),
      SubClassOfAxiom(B, X1),
      SubClassOfAxiom(C, X2),
      SubClassOfAxiom(D, X1),
      SubClassOfAxiom(X0, X1)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Domain1 [from Pellet]") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, A),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, Thing))
    )
    val expected = Set(
      SubClassOfAxiom(B, A)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Domain2 [from Pellet]") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, C),
      ObjectPropertyDomainAxiom(R, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, Thing))
    )
    val expected = Set(
      SubClassOfAxiom(B, A),
      SubClassOfAxiom(B, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("DomainAbsorption [from Pellet]") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, A),
      SubClassOfAxiom(E, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, B), ObjectSomeValuesFrom(R, Thing)), C)
    )
    val expected = Set(
      SubClassOfAxiom(E, A)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("DomainBottom [from Pellet]") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, Nothing),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B))
    )
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

  test("ReflexiveRole [from Pellet]") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, A),
      ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(B, C)),
      ReflexiveObjectPropertyAxiom(R)
    )
    val expected = Set(
      EquivalentClassesAxiom(Thing, A, B, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Range1 [from Pellet]") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, A),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), D)
    )
    val expected = Set(
      SubClassOfAxiom(B, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Range2 [from Pellet]") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(A, B)), E)
    )
    val expected = Set(
      SubClassOfAxiom(C, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Range3 [from Pellet]") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), E)
    )
    val expected = Set(
      SubClassOfAxiom(C, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Range5 [from Pellet]") {
    val input = Set(
      EquivalentClassesAxiom(D, ObjectSomeValuesFrom(R, C)),
      ObjectPropertyRangeAxiom(R, A),
      SubClassOfAxiom(B, ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, C))),
      SubClassOfAxiom(C, A)
    )
    val expected = Set(
      SubClassOfAxiom(B, A),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(C, A)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("DomainNormalization1 [from Pellet]") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, ObjectIntersectionOf(B, C)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(R, X1)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, C), E),
      ObjectPropertyDomainAxiom(R, ObjectSomeValuesFrom(S, C))
    )
    val expected = Set(
      SubClassOfAxiom(D, B),
      SubClassOfAxiom(D, C),
      SubClassOfAxiom(D, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("RangeNormalization1 [from Pellet]") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, A),
      ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(B, C)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(R, X1)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectIntersectionOf(A, B), C)), E)
    )
    val expected = Set(
      SubClassOfAxiom(D, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("RangeNormalization2 [from Pellet]") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, ObjectSomeValuesFrom(S, A)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, X0)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(S, A)), C)
    )
    val expected = Set(
      SubClassOfAxiom(B, C)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("DomainAndRange [from Pellet]") {
    val input = Set(
      ObjectPropertyDomainAxiom(S, B),
      ObjectPropertyRangeAxiom(R, A),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, Thing)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), ObjectSomeValuesFrom(S, Thing))
    )
    val expected = Set(
      SubClassOfAxiom(C, B)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Range4 [from Pellet]") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectIntersectionOf(B, C), D),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, D), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("SomeConjunction [from Pellet]") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C, D))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C)), E)
    )
    val expected = Set(
      SubClassOfAxiom(A, E)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("DisjointRange [from Pellet]") {
    val input = Set(
      DisjointClassesAxiom(B, C),
      ObjectPropertyRangeAxiom(R, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B))
    )
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

  test("DisjointRangeSuper [from Pellet]") {
    val input = Set(
      DisjointClassesAxiom(D, C),
      ObjectPropertyRangeAxiom(R, C),
      SubClassOfAxiom(A, E),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(B, F)
    )
    val expected = Set(
      EquivalentClassesAxiom(Nothing, A),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(B, F)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Ticket465 [from Pellet]") {
    val input = Set(
      EquivalentClassesAxiom(D, ObjectIntersectionOf(ObjectSomeValuesFrom(T, E), A)),
      SubClassOfAxiom(B, A),
      SubClassOfAxiom(C, B),
      SubClassOfAxiom(F, C),
      SubClassOfAxiom(F, ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectSomeValuesFrom(S, G), E))),
      SubObjectPropertyOfAxiom(R, T)
    )
    val expected = Set(
      SubClassOfAxiom(B, A),
      SubClassOfAxiom(C, B),
      SubClassOfAxiom(D, A),
      SubClassOfAxiom(F, C),
      SubClassOfAxiom(F, D)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("NestedSubProperty [from Pellet]") {
    val input = Set(
      EquivalentClassesAxiom(A, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(T, C))),
      EquivalentClassesAxiom(B, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(S, C))),
      SubObjectPropertyOfAxiom(T, S)
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

  test("endocarditis [from ELK]") {
    // The example from "CEL---A Polynomial-Time Reasoner for Life Science Ontologies", F. Baader, C. Lutz,
    // and B. Suntisrivaraporn, IJCAR 2006, with some axioms modified to match the test case used in ELK.
    val BodyValve = OWLClass(IRI(p, "BodyValve"))
    val Disease = OWLClass(IRI(p, "Disease"))
    val Endocarditis = OWLClass(IRI(p, "Endocarditis"))
    val Endocardium = OWLClass(IRI(p, "Endocardium"))
    val Heart = OWLClass(IRI(p, "Heart"))
    val HeartDisease = OWLClass(IRI(p, "HeartDisease"))
    val HeartValve = OWLClass(IRI(p, "HeartValve"))
    val Inflammation = OWLClass(IRI(p, "Inflammation"))
    val IsStateNeedsTreatment = OWLClass(IRI(p, "IsStateNeedsTreatment"))
    val Pericarditis = OWLClass(IRI(p, "Pericarditis"))
    val Pericardium = OWLClass(IRI(p, "Pericardium"))
    val Tissue = OWLClass(IRI(p, "Tissue"))
    val actsOn = ObjectProperty(IRI(p, "acts-on"))
    val containedIn = ObjectProperty(IRI(p, "contained-in"))
    val hasLocation = ObjectProperty(IRI(p, "has-location"))
    val partOf = ObjectProperty(IRI(p, "part-of"))
    val input = Set(
      EquivalentClassesAxiom(HeartDisease, ObjectIntersectionOf(ObjectSomeValuesFrom(hasLocation, Heart), Disease)),
      SubClassOfAxiom(Endocarditis, ObjectIntersectionOf(ObjectSomeValuesFrom(hasLocation, Endocardium), Inflammation)),
      SubClassOfAxiom(Endocardium, ObjectIntersectionOf(ObjectSomeValuesFrom(containedIn, HeartValve), Tissue)),
      SubClassOfAxiom(HeartValve, ObjectIntersectionOf(ObjectSomeValuesFrom(partOf, Heart), BodyValve)),
      SubClassOfAxiom(Inflammation, ObjectIntersectionOf(ObjectSomeValuesFrom(actsOn, Tissue), Disease)),
      SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(hasLocation, HeartValve), HeartDisease), IsStateNeedsTreatment),
      SubClassOfAxiom(Pericarditis, ObjectIntersectionOf(ObjectSomeValuesFrom(hasLocation, Pericardium), Inflammation)),
      SubClassOfAxiom(Pericardium, ObjectIntersectionOf(ObjectSomeValuesFrom(containedIn, Heart), Tissue)),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(hasLocation, containedIn), hasLocation),
      SubObjectPropertyOfAxiom(partOf, containedIn)
    )
    val expected = Set(
      SubClassOfAxiom(Endocarditis, HeartDisease),
      SubClassOfAxiom(Endocarditis, Inflammation),
      SubClassOfAxiom(Endocarditis, IsStateNeedsTreatment),
      SubClassOfAxiom(Endocardium, Tissue),
      SubClassOfAxiom(HeartDisease, Disease),
      SubClassOfAxiom(HeartValve, BodyValve),
      SubClassOfAxiom(Inflammation, Disease),
      SubClassOfAxiom(Pericarditis, HeartDisease),
      SubClassOfAxiom(Pericarditis, Inflammation),
      SubClassOfAxiom(Pericardium, Tissue)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

  test("Heart [from Pellet]") {
    // The example from "CEL---A Polynomial-Time Reasoner for Life Science Ontologies", F. Baader, C. Lutz,
    // and B. Suntisrivaraporn, IJCAR 2006, with some axioms added to match the test case used in Pellet.
    val BodyValve = OWLClass(IRI(p, "BodyValve"))
    val BodyWall = OWLClass(IRI(p, "BodyWall"))
    val CriticalDisease = OWLClass(IRI(p, "CriticalDisease"))
    val Disease = OWLClass(IRI(p, "Disease"))
    val Endocarditis = OWLClass(IRI(p, "Endocarditis"))
    val Endocardium = OWLClass(IRI(p, "Endocardium"))
    val Heart = OWLClass(IRI(p, "Heart"))
    val HeartDisease = OWLClass(IRI(p, "HeartDisease"))
    val HeartValve = OWLClass(IRI(p, "HeartValve"))
    val HeartWall = OWLClass(IRI(p, "HeartWall"))
    val Inflammation = OWLClass(IRI(p, "Inflammation"))
    val Tissue = OWLClass(IRI(p, "Tissue"))
    val actsOn = ObjectProperty(IRI(p, "acts-on"))
    val containedIn = ObjectProperty(IRI(p, "contained-in"))
    val hasLocation = ObjectProperty(IRI(p, "has-location"))
    val partOf = ObjectProperty(IRI(p, "part-of"))
    val input = Set(
      EquivalentClassesAxiom(HeartDisease, ObjectIntersectionOf(Disease, ObjectSomeValuesFrom(hasLocation, Heart))),
      SubClassOfAxiom(Endocarditis, ObjectIntersectionOf(Inflammation, ObjectSomeValuesFrom(hasLocation, Endocardium))),
      SubClassOfAxiom(Endocardium, ObjectIntersectionOf(Tissue, ObjectSomeValuesFrom(containedIn, HeartWall), ObjectSomeValuesFrom(containedIn, HeartValve))),
      SubClassOfAxiom(HeartValve, ObjectIntersectionOf(BodyValve, ObjectSomeValuesFrom(partOf, Heart))),
      SubClassOfAxiom(HeartWall, ObjectIntersectionOf(BodyWall, ObjectSomeValuesFrom(partOf, Heart))),
      SubClassOfAxiom(Inflammation, ObjectIntersectionOf(Disease, ObjectSomeValuesFrom(actsOn, Tissue))),
      SubClassOfAxiom(ObjectIntersectionOf(HeartDisease, ObjectSomeValuesFrom(hasLocation, HeartValve)), CriticalDisease),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(hasLocation, containedIn), hasLocation),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(partOf, partOf), partOf),
      SubObjectPropertyOfAxiom(partOf, containedIn)
    )
    val expected = Set(
      SubClassOfAxiom(Endocarditis, CriticalDisease),
      SubClassOfAxiom(Endocarditis, HeartDisease),
      SubClassOfAxiom(Endocarditis, Inflammation),
      SubClassOfAxiom(Endocardium, Tissue),
      SubClassOfAxiom(HeartDisease, Disease),
      SubClassOfAxiom(HeartValve, BodyValve),
      SubClassOfAxiom(HeartWall, BodyWall),
      SubClassOfAxiom(Inflammation, Disease)
    )
    computeTaxonomy(input) match {
      case None =>
        fail("Ontology is inconsistent")
      case Some(actual) =>
        assert(decorate(actual) === decorate(expected), "Computed taxonomy differs from known taxonomy.")
    }
  }

}
