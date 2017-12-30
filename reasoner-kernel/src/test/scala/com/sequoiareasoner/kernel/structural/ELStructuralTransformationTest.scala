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
import com.sequoiareasoner.kernel.owl.iri._
import com.sequoiareasoner.kernel.owl.model._
import org.scalatest.{FunSuite, RandomTestOrder}


/** Structural transformation tests of OWL axioms that fall within OWL EL.
  *
  * Each test takes a set of OWL axioms and structurally transforms the axioms into a new set of ontology clauses.
  * The set of clauses produced is compared for equivalence with a known set of correctly transformed clauses,
  * up to variable renaming, function symbol renaming, and auxiliary IRI renaming.
  *
  * Each set of clauses produced by the structural transformer are also asserted to be horn.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class ELStructuralTransformationTest extends FunSuite with RandomTestOrder {
  import ClauseSetEquivalenceUtilities._
  import CommonNames._
  import OWLAxiomBuilder._

  // Passes the current suite implicitly to the decorated sets.
  private[this] implicit val self = this

  // Dummy ontology to allow for the construction of concepts and roles.
  private[this] implicit val dlOntology: DLOntology = null

  private[this] val AuxAll0 = IRI.all()
  private[this] val AuxAll1 = IRI.all()
  private[this] val AuxAll2 = IRI.all()
  private[this] val AuxAll3 = IRI.all()
  private[this] val AuxAll4 = IRI.all()
  private[this] val AuxSome0 = IRI.some()
  private[this] val AuxSome1 = IRI.some()

  private[this] def isHorn(c: OntologyClause): Boolean = c.isHorn
  private[this] def nonEmptyBody(c: OntologyClause): Boolean = c.body.nonEmpty

  // TODO: optimise aux:all cases by inlining the auxiliary clause.
  // For example
  // SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(A, B)), E) -->
  // OntologyClause(Body(Concept(A, x), Concept(B, x), Role(R, z1, x)), Head(Concept(AuxAll0, z1))),
  // OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),

  // FIXME: optimize property ranges to derive Concept(A, x) in head.
  // ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(A, B)) -->
  // OntologyClause(Body(Role(R, z1, x)), Head(Concept(A, x)))
  // OntologyClause(Body(Role(R, z1, x)), Head(Concept(B, x)))

  // In general, if the central variable is in the head, it cannot be pushed back or forwards.

  test("corresponds to: ancestors") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(C, D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: bottom") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, Nothing),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, A)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head()),
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Role(S, x, f2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: ancestors, bottom") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(B, Nothing),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, A)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f4))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f4))),
      OntologyClause(Body(Concept(B, x)), Head()),
      OntologyClause(Body(Concept(B, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, f3))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x)), Head(Role(S, x, f3))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: conjunctions") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(ObjectIntersectionOf(B, C, D), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(B, x), Concept(C, x), Concept(D, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: disjoint") {
    val input = Set(
      DisjointClassesAxiom(A, B, C),
      SubClassOfAxiom(A, C),
      SubClassOfAxiom(B, C),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(B, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: disjointSelf") {
    val input = Set(
      DisjointClassesAxiom(A, B, A, A),
      DisjointClassesAxiom(C, C),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head()),
      OntologyClause(Body(Concept(C, x)), Head()),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: simple equivalent") {
    val input = Set(
      EquivalentClassesAxiom(B, C),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(B, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: existentials") {
    val input = Set(
      EquivalentClassesAxiom(B, C),
      ObjectPropertyDomainAxiom(S, E),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, C), D),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(B, x))),
      OntologyClause(Body(Role(S, x, z1)), Head(Concept(E, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(S, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(R, x, z1)), Head(Role(S, x, z1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: forest simplified") {
    val input = Set(
      SubClassOfAxiom(A, X),
      SubClassOfAxiom(B, X),
      SubClassOfAxiom(C, X),
      SubClassOfAxiom(D, X),
      SubClassOfAxiom(E, X),
      SubClassOfAxiom(F, X),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(X, x))),
      OntologyClause(Body(Concept(F, x)), Head(Concept(X, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: forest") {
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
      TransitiveObjectPropertyAxiom(hasPart),
    )
    // The output is too large for the test framework to check for equivalence, so just check the following instead.
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(actual forall nonEmptyBody)
  }

  test("corresponds to: inconsistent") {
    val input = Set(
      EquivalentClassesAxiom(A, C),
      ObjectPropertyDomainAxiom(T, Nothing),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(T, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, B), A),
      SubClassOfAxiom(Thing, ObjectSomeValuesFrom(R, B)),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, x))),
      OntologyClause(Body(Role(T, x, z1)), Head()),
      OntologyClause(Body(Concept(C, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Role(T, x, f1))),
      OntologyClause(Body(Role(S, z1, x), Concept(B, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(A, x))),
      OntologyClause(Body(), Head(Concept(B, f2))),
      OntologyClause(Body(), Head(Role(R, x, f2))),
      OntologyClause(Body(Role(R, x, z3)), Head(Role(S, x, z3))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: kangaroo") { // FIXME: Update expected output for new clausification (try inlining).
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
      SubClassOfAxiom(KangarooInfant, ObjectIntersectionOf(ObjectSomeValuesFrom(livesIn, Pouch), Kangaroo)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(Beast, x)), Head(Concept(Animal, x))),
      OntologyClause(Body(Concept(Beast, x)), Head(Concept(Irrational, x))),
      OntologyClause(Body(Concept(Human, x)), Head(Concept(Animal, x))),
      OntologyClause(Body(Concept(Human, x)), Head(Concept(Rational, x))),
      OntologyClause(Body(Concept(Irrational, x), Concept(Rational, x)), Head()),
      OntologyClause(Body(Concept(Kangaroo, x)), Head(Concept(Beast, x))),
      OntologyClause(Body(Concept(KangarooInfant, x)), Head(Concept(Kangaroo, x))),
      OntologyClause(Body(Concept(KangarooInfant, x)), Head(Concept(Pouch, f1))),
      OntologyClause(Body(Concept(KangarooInfant, x)), Head(Role(livesIn, x, f1))),
      OntologyClause(Body(Concept(MaternityKangaroo, x)), Head(Concept(Kangaroo, x))),
      OntologyClause(Body(Concept(MaternityKangaroo, x)), Head(Concept(KangarooInfant, f2))),
      OntologyClause(Body(Concept(MaternityKangaroo, x)), Head(Concept(Pouch, f3))),
      OntologyClause(Body(Concept(MaternityKangaroo, x)), Head(Role(hasBodyPart, x, f3))),
      OntologyClause(Body(Concept(MaternityKangaroo, x)), Head(Role(hasChild, x, f2))),
      OntologyClause(Body(Concept(Parent, x)), Head(Concept(AuxSome0, f4))), // TODO: this axiom should have been removed
      OntologyClause(Body(Concept(Parent, x)), Head(Role(hasChild, x, f4))),
      OntologyClause(Body(Concept(Rational, x), Concept(Irrational, x)), Head()),
      OntologyClause(Body(Role(AuxAll0, x, z7), Role(AuxAll1, x,z8), Concept(Kangaroo, x)), Head(Concept(MaternityKangaroo, x))),
      OntologyClause(Body(Role(hasBodyPart, z1, x), Concept(Pouch, x)), Head(Role(AuxAll0, z1, x))),
      OntologyClause(Body(Role(hasChild, x, z11)), Head(Concept(Human, x))),
      OntologyClause(Body(Role(hasChild, x, z9)), Head(Concept(Parent, x))),
      OntologyClause(Body(Role(hasChild, z1, x), Concept(KangarooInfant, x)), Head(Role(AuxAll1, z1, x))),
      OntologyClause(Body(Role(hasChild, z10, x)), Head(Concept(Human, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("simple chain clausification") {
    val input = Set(
      SubClassOfAxiom(ObjectSomeValuesFrom(T, Thing), B),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2), T),
    )
    val expected = Set(
      OntologyClause(Body(Role(R1, z2, x), Role(R2, x, z4)), Head(Concept(B, z2))),
      OntologyClause(Body(Role(T, x, z3)), Head(Concept(B, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: propertyChains") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R1, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R2, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R3, D)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(R4, E)),
      SubClassOfAxiom(ObjectIntersectionOf(Thing, ObjectSomeValuesFrom(T, Thing)), X),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2, S), T),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R3, R4), S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R1, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R2, x, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f3))),
      OntologyClause(Body(Concept(C, x)), Head(Role(R3, x, f3))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(E, f4))),
      OntologyClause(Body(Concept(D, x)), Head(Role(R4, x, f4))),
      OntologyClause(Body(Role(AuxAll0, x, z1), Role(R2, z2, x)), Head(Concept(AuxAll1, z2))),
      OntologyClause(Body(Role(R1, z3, x), Concept(AuxAll1, x)), Head(Concept(X, z3))),
      OntologyClause(Body(Role(R3, z4, x), Concept(AuxAll2, x)), Head(Role(AuxAll0, z4, x))),
      OntologyClause(Body(Role(R4, x, z5)), Head(Concept(AuxAll2, x))),
      OntologyClause(Body(Role(S, x, z6), Role(R2, z7, x)), Head(Concept(AuxAll1, z7))),
      OntologyClause(Body(Role(T, x, z8)), Head(Concept(X, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("simple reflexive role") {
    val input = Set(
      ReflexiveObjectPropertyAxiom(R),
      EquivalentClassesAxiom(B, ObjectSomeValuesFrom(R, A)),
      EquivalentClassesAxiom(D, ObjectSomeValuesFrom(S, C)),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), S),
    )
    val expected = Set(
      OntologyClause(Body(), Head(Role(R, x, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(A, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, z1, x), Concept(A, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(D, x)), Head(Role(S, x, f2))),
      OntologyClause(Body(Concept(AuxAll2, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(AuxAll3, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(S, z1, x), Concept(C, x)), Head(Concept(AuxAll2, z1))),
      OntologyClause(Body(Concept(AuxAll1, x)), Head(Concept(AuxAll4, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll4, x)), Head(Concept(AuxAll3, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll1, z1))),

    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: top") {
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(Thing, C),
    )
    val expected = Set(
      OntologyClause(Body(), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL1"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, ObjectSomeValuesFrom(R, C))),
      SubClassOfAxiom(ObjectIntersectionOf(B, D), E),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, Thing), D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x), Concept(D, x)), Head(Concept(E, x))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(D, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL2"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, D), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(D, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL3a"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C, E),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(D, E)), F),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(E, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(D, x), Concept(E, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(F, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL3b"){
    val input = Set(
      EquivalentClassesAxiom(G, ObjectIntersectionOf(D, E)),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(C, E),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, G), F),
    )
    val expected = Set(
      OntologyClause(Body(Concept(G, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(G, x)), Head(Concept(E, x))),
      OntologyClause(Body(Concept(D, x), Concept(E, x)), Head(Concept(G, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(E, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(G, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(F, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL3c"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, D))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, C), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL4"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, C, D)),
      SubClassOfAxiom(ObjectIntersectionOf(C, D), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x), Concept(D, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL5a"){
    val input = Set(
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, F),
      SubClassOfAxiom(F, ObjectSomeValuesFrom(R, G)),
      SubClassOfAxiom(G, C),
      SubClassOfAxiom(G, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectSomeValuesFrom(R, B), C))), D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(F, x))),
      OntologyClause(Body(Concept(F, x)), Head(Concept(G, f1))),
      OntologyClause(Body(Concept(F, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(G, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(G, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(G, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Role(AuxAll1, x, z1), Role(AuxAll2, x, z2), Concept(C, x)), Head(Concept(D, z2))),
      OntologyClause(Body(Role(R, x, z3), Concept(A, x)), Head(Role(AuxAll2, z3, x))),
      OntologyClause(Body(Role(R, z4, x), Concept(AuxAll0, x)), Head(Role(AuxAll1, z4, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: EL5b"){ // FIXME: Update expected output for new clausification (try inlining).
    val input = Set(
      EquivalentClassesAxiom(X1, ObjectSomeValuesFrom(R, X2)),
      EquivalentClassesAxiom(X2, ObjectIntersectionOf(X3, C)),
      EquivalentClassesAxiom(X3, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, F),
      SubClassOfAxiom(F, ObjectSomeValuesFrom(R, G)),
      SubClassOfAxiom(G, C),
      SubClassOfAxiom(G, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectIntersectionOf(A, X1), D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(X1, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(F, x))),
      OntologyClause(Body(Concept(F, x)), Head(Concept(G, f1))),
      OntologyClause(Body(Concept(F, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(G, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(G, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(G, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(X1, x)), Head(Concept(X2, f3))),
      OntologyClause(Body(Concept(X1, x)), Head(Role(R, x, f3))),
      OntologyClause(Body(Concept(X2, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(X2, x)), Head(Concept(X3, x))),
      /* If the positive occurrance of ObjectSomeValuesFrom(R, B) is correctly internalised, then the following two
       * axioms will reuse the function symbol f2 from above rather than a use a fresh one. */
      OntologyClause(Body(Concept(X3, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(X3, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(X3, x), Concept(C, x)), Head(Concept(X2, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(X2, x)), Head(Concept(X1, z1))),
      OntologyClause(Body(Role(R, z2, x), Concept(B, x)), Head(Concept(X3, z2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL6"){
    val input = Set(
      SubClassOfAxiom(E, A),
      SubClassOfAxiom(E, ObjectSomeValuesFrom(R, G)),
      SubClassOfAxiom(G, B),
      SubClassOfAxiom(G, C),
      SubClassOfAxiom(ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C))), D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(AuxAll0, x), Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(G, f1))),
      OntologyClause(Body(Concept(E, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(G, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(G, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(B, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL7"){
    val input = Set(
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(ObjectIntersectionOf(A, B), ObjectIntersectionOf(C, D, Thing)),
      SubClassOfAxiom(ObjectIntersectionOf(A, C), E),
      SubClassOfAxiom(ObjectIntersectionOf(A, D, Thing), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(D, x)), Head(Concept(E, x))),
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x), Concept(C, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: EL8"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, C),
      SubClassOfAxiom(C, D),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(D, Thing)), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(D, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: ELNormalization1"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(C, B)), D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x), Concept(B, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(D, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: ELNormalization2"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, ObjectIntersectionOf(C, D))),
      SubClassOfAxiom(ObjectIntersectionOf(C, ObjectIntersectionOf(B, D)), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(C, x), Concept(B, x), Concept(D, x)), Head(Concept(E, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: ELNormalization3"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(B, ObjectIntersectionOf(C, D, ObjectIntersectionOf(E, F)))),
      SubClassOfAxiom(ObjectIntersectionOf(ObjectIntersectionOf(C, F), ObjectIntersectionOf(B, D, E)), G),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(E, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(F, x))),
      OntologyClause(Body(Concept(C, x), Concept(F, x), Concept(B, x), Concept(D, x), Concept(E, x)), Head(Concept(G, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Bottom1"){
    val input = Set(
      SubClassOfAxiom(A, Nothing),
      SubClassOfAxiom(C, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(Nothing, D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head()),
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(B, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Bottom2"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(C, Nothing),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), C),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(C, x)), Head()),
      OntologyClause(Body(Role(R, z1, x), Concept(B, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Top1"){
    val input = Set(
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), D),
      SubClassOfAxiom(Thing, A),
    )
    val expected = Set(
      OntologyClause(Body(Concept(C, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, z1, x), Concept(A, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(D, x))),
      OntologyClause(Body(), Head(Concept(A, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: BottomWithObjectSomeValuesFrom1"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, Nothing),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head()),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: BottomWithObjectSomeValuesFrom2"){
    val input = Set(
      SubClassOfAxiom(A, Nothing),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, A)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, B)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head()),
      OntologyClause(Body(Concept(B, x)), Head(Concept(A, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Role(S, x, f2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Disjoint"){
    val input = Set(
      DisjointClassesAxiom(C, D),
      EquivalentClassesAxiom(C, D),
      SubClassOfAxiom(A, B),
      SubClassOfAxiom(ObjectIntersectionOf(A, B), Nothing),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(C, x), Concept(D, x)), Head()),
      OntologyClause(Body(Concept(D, x)), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DisjointWithObjectSomeValuesFrom1 simplified"){
    val input = Set(
      DisjointClassesAxiom(ObjectSomeValuesFrom(R, C), D),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), ObjectSomeValuesFrom(R, C)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(AuxAll0, x), Concept(D, x)), Head()),
      OntologyClause(Body(Concept(AuxAll1, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(AuxAll1, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Role(R, z1, x), Concept(B, x)), Head(Concept(AuxAll1, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DisjointWithObjectSomeValuesFrom1"){
    val input = Set(
      DisjointClassesAxiom(ObjectSomeValuesFrom(S, C), D),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), ObjectSomeValuesFrom(R, C)),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(AuxAll0, x, z1)), Head(Concept(C, f2))),
      OntologyClause(Body(Role(AuxAll0, x, z2)), Head(Role(R, x, f2))),
      OntologyClause(Body(Role(AuxAll1,x, z3), Concept(D, x)), Head()),
      OntologyClause(Body(Role(R, x, z4)), Head(Role(S, x, z4))),
      OntologyClause(Body(Role(R, z5, x), Concept(B, x)), Head(Role(AuxAll0, z5, x))),
      OntologyClause(Body(Role(S, z6, x), Concept(C, x)), Head(Role(AuxAll1, z6, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DisjointWithObjectSomeValuesFrom2"){
    val input = Set(
      DisjointClassesAxiom(B, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C))),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Roles1a"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, B), C),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(S, z1, x), Concept(B, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(R, x, z2)), Head(Role(S, x, z2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Roles1b"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(D, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(ObjectIntersectionOf(D, ObjectSomeValuesFrom(S, B)), C),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(D, x), Role(S, x, z1)), Head(Role(AuxAll0, z1, x))),
      OntologyClause(Body(Concept(D, x), Role(R, x, z1)), Head(Role(AuxAll0, z1, x))),
      OntologyClause(Body(Role(AuxAll0, x, z1), Concept(B, x)), Head(Concept(C, z1))) // FIXME: this axiom is being generated twice
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: Roles2a simplified 1"){ // FIXME: Update expected output for new clausification.
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(Thing, ObjectUnionOf(C, ObjectAllValuesFrom(R, ObjectComplementOf(B)))),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(AuxSome0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(R, z2, x), Concept(AuxAll0, x)), Head(Concept(C, z2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: Roles2a simplified 2"){ // FIXME: Update expected output for new clausification.
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), C),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(AuxSome0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll0, x)), Head(Concept(C, z1))),
      OntologyClause(Body(Role(R, z2, x), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: Roles2a"){ // FIXME: Update expected output for new clausification (try inlining).
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectSomeValuesFrom(R, B))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), C),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R),
      SubObjectPropertyOfAxiom(S, R),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(AuxSome0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Role(R, z2, x), Concept(AuxAll0, x)), Head(Concept(C, z2))),
      OntologyClause(Body(Role(S, z3, x), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z3))),
      OntologyClause(Body(Role(S, z4, x), Concept(AuxAll0, x)), Head(Concept(C, z4))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Roles2b"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(F, ObjectSomeValuesFrom(P, B))),
      SubClassOfAxiom(B, ObjectIntersectionOf(G, ObjectSomeValuesFrom(R, C))),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(P, D), E),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(R, S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(D, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(F, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(P, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(G, x))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f3))),
      OntologyClause(Body(Concept(C, x)), Head(Role(S, x, f3))),
      OntologyClause(Body(Role(P, z1, x), Concept(AuxAll0, x)), Head(Concept(E, z1))),
      OntologyClause(Body(Role(R, z2, x)), Head(Role(S, z2, x))),
      OntologyClause(Body(Role(S, x, z3), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z3))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Roles2c"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(P, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(S, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(S, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, D), E),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), R),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(P, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Role(S, x, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f3))),
      OntologyClause(Body(Concept(C, x)), Head(Role(S, x, f3))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Role(AuxAll1, x, z2), Role(P, z2,x)), Head(Concept(E, z2))),
      OntologyClause(Body(Role(S, z1, x), Concept(AuxAll0, x)), Head(Role(AuxAll1, z1, x))),
      OntologyClause(Body(Role(R, z3, x), Concept(D, x)), Head(Concept(E, z3))),
      OntologyClause(Body(Role(S, z4, x), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z4))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: Roles3a"){ // FIXME: Update expected output for new clausification (try inlining).
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(P, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(S, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(P, D), E),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S, R), P),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(P, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Role(S, x, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f3))),
      OntologyClause(Body(Concept(C, x)), Head(Role(R, x, f3))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Role(P, z1, x), Concept(AuxAll0, x)), Head(Concept(E, z1))),
      OntologyClause(Body(Role(R, z1, x), Concept(AuxAll0, x)), Head(Concept(AuxAll1, z1))),
      OntologyClause(Body(Role(S, z1, x), Concept(AuxAll1, x)), Head(Concept(AuxAll0, z1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Roles3b"){
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(P, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(T, C)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(T, E)),
      SubClassOfAxiom(E, ObjectSomeValuesFrom(S, F)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, F), G),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, T, R), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, T, S), S),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(P, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Role(T, x, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f3))),
      OntologyClause(Body(Concept(C, x)), Head(Role(R, x, f3))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(E, f4))),
      OntologyClause(Body(Concept(D, x)), Head(Role(T, x, f4))),
      OntologyClause(Body(Concept(E, x)), Head(Concept(F, f5))),
      OntologyClause(Body(Concept(E, x)), Head(Role(S, x, f5))),
      OntologyClause(Body(Concept(F, x), Role(S, z1, x)), Head(Concept(G, z1))),
      OntologyClause(Body(Concept(F, x), Role(S, z2, x)), Head(Concept(AuxAll0, z2))),
      OntologyClause(Body(Role(P, z3, x), Role(AuxAll1, x, z4)), Head(Concept(AuxAll0, z3))),
      OntologyClause(Body(Role(P, z5, x), Role(AuxAll1, x, z6)), Head(Concept(G, z5))),
      OntologyClause(Body(Role(R, z7, x), Role(AuxAll1, x, z8)), Head(Concept(AuxAll0, z7))),
      OntologyClause(Body(Role(T, z9, x), Concept(AuxAll0, x)), Head(Role(AuxAll1, z9, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Roles4"){
    val input = Set(
      SubClassOfAxiom(A, ObjectIntersectionOf(X0, ObjectSomeValuesFrom(P, B))),
      SubClassOfAxiom(B, ObjectIntersectionOf(X1, ObjectSomeValuesFrom(T, C))),
      SubClassOfAxiom(C, ObjectIntersectionOf(X2, ObjectSomeValuesFrom(R, D))),
      SubClassOfAxiom(D, ObjectIntersectionOf(X1, ObjectSomeValuesFrom(S, E))),
      SubClassOfAxiom(ObjectIntersectionOf(X0, ObjectSomeValuesFrom(P, E)), F),
      SubClassOfAxiom(X0, X1),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, T, R, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(T, R), S),
    )
    /*val expected = Set(
      OntologyClause(Body(Concept(D, x)), Head(Concept(E, f70))
      OntologyClause(Body(Role(all:111, x, z79), Role(T, z77, x)), Head(Concept(all:107, z77))
      OntologyClause(Body(Concept(D, x)), Head(Concept(X1, x))
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f68))
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f67))
      OntologyClause(Body(Role(all:111, x, z78), Role(T, z75, x)), Head(Concept(all:107, z75))
      OntologyClause(Body(Role(R, z1, x), Concept(all:107, x)), Head(Role(all:111, z1, x))
      OntologyClause(Body(Concept(C, x)), Head(Concept(X2, x))
      OntologyClause(Body(Concept(E, x)), Head(Concept(all:107, x))
      OntologyClause(Body(Concept(A, x)), Head(Role(P, x, f68))
      OntologyClause(Body(Concept(B, x)), Head(Concept(X1, x))
      OntologyClause(Body(Concept(X0, x)), Head(Concept(X1, x))
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f69))
      OntologyClause(Body(Role(P, x, z1), Concept(X0, x)), Head(Role(all:110, z1, x))
      OntologyClause(Body(Concept(C, x)), Head(Role(R, x, f67))
      OntologyClause(Body(Concept(A, x)), Head(Concept(X0, x))
      OntologyClause(Body(Concept(B, x)), Head(Role(T, x, f69))
      OntologyClause(Body(Concept(all:107, x), Role(all:110, x, z80)), Head(Concept(F, z80))
      OntologyClause(Body(Concept(D, x)), Head(Role(S, x, f70))
      OntologyClause(Body(Role(S, z76, x), Concept(all:107, x)), Head(Concept(all:107, z76)),
    )*/
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Domain1") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, A),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, Thing)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Domain2") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, C),
      ObjectPropertyDomainAxiom(R, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, Thing)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(C, x))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, x))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(B, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: DomainAbsorption") { // FIXME: this optimization is missing.
    val input = Set(
      ObjectPropertyDomainAxiom(R, A),
      SubClassOfAxiom(E, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, B), ObjectSomeValuesFrom(R, Thing)), C),
    )
    val expected = Set(
      OntologyClause(Body(Concept(E, x)), Head(Concept(D, f1))),
      OntologyClause(Body(Concept(E, x)), Head(Role(R, x, f1))),
      // We check that the negative occurrence of ObjectSomeValuesFrom(R, Thing) has been optimized away.
      OntologyClause(Body(Role(R, z1, x), Concept(B, x)), Head(Concept(C, z1))),
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(A, z1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DomainBottom") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, Nothing),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, x, z1)), Head()),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Range1") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, A),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, C)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), D),
    )
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, z1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, z1, x), Concept(A, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(D, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Range2") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(A, B)), E),
    )
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, z1))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(B, z1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x), Concept(B, x), Role(R, z1, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Range3") {
    val input = Set(
      ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(A, B)),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, D)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), E),
    )
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, z1))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(B, z1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x), Role(R, z1, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: Range5") { // FIXME: Update expected output for new clausification (try inlining).
    val input = Set(
      EquivalentClassesAxiom(D, ObjectSomeValuesFrom(R, C)),
      ObjectPropertyRangeAxiom(R, A),
      SubClassOfAxiom(B, ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, C))),
      SubClassOfAxiom(C, A),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(C, x), Role(R, z1, x)), Head(Concept(D, z1))),
      /* If the positive occurrance of ObjectSomeValuesFrom(R, C) is correctly internalised, then the following two
       * axioms will reuse the function symbol f1 from above rather than a use a fresh one. */
      OntologyClause(Body(Concept(D, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(D, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(A, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DomainNormalization1") {
    val input = Set(
      ObjectPropertyDomainAxiom(R, ObjectIntersectionOf(B, C)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(R, X1)),
      SubClassOfAxiom(ObjectSomeValuesFrom(S, C), E),
      ObjectPropertyDomainAxiom(R, ObjectSomeValuesFrom(S, C)),
    )
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(B, x))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(X1, f1))),
      OntologyClause(Body(Concept(D, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(S, z1, x), Concept(C, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(C, f2))),
      OntologyClause(Body(Role(R, x, z1)), Head(Role(S, x, f2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: RangeNormalization1") { // FIXME: Update expected output for new clausification (try inlining).
    val input = Set(
      ObjectPropertyRangeAxiom(R, A),
      ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(B, C)),
      SubClassOfAxiom(D, ObjectSomeValuesFrom(R, X1)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectIntersectionOf(A, B), C)), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(B, x), Concept(C, x), Role(R, z1, x)), Head(Concept(E, z1))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(X1, f1))),
      OntologyClause(Body(Concept(D, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(B, x))),
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(C, x))),
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(A, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: RangeNormalization2") { // FIXME: generating too many auxillary clauses.
    val input = Set(
      ObjectPropertyRangeAxiom(R, ObjectSomeValuesFrom(S, A)),
      SubClassOfAxiom(B, ObjectSomeValuesFrom(R, X0)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(S, A)), C),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(S, z1, x)), Head(Role(AuxAll0, z1, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(X0, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(AuxAll0, x, z2), Role(R, z3, x)), Head(Concept(C, z3))),
      OntologyClause(Body(Role(R, z4, x)), Head(Concept(A, f2))),
      OntologyClause(Body(Role(R, z5, x)), Head(Role(S, x, f2))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DomainAndRange") {
    val input = Set(
      ObjectPropertyDomainAxiom(S, B),
      ObjectPropertyRangeAxiom(R, A),
      SubClassOfAxiom(C, ObjectSomeValuesFrom(R, Thing)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, A), ObjectSomeValuesFrom(S, Thing)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(R, z1,x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(C, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Role(R, x, z2)), Head(Concept(A, z2))),
      OntologyClause(Body(Role(S, x, z3)), Head(Concept(B, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("corresponds to: Range4") { // TODO: Test when auxiliary clauses are not generated.
    val input = Set(
      ObjectPropertyRangeAxiom(R, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectIntersectionOf(B, C), D),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, D), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x), Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(D, x), Role(R, z1, x)), Head(Concept(E, z1))),
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(C, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: SomeConjunction") {
    val input = Set(
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C, D))),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectIntersectionOf(B, C)), E),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x), Concept(C, x), Role(R, z1, x)), Head(Concept(AuxAll0, z1))),
      OntologyClause(Body(Concept(AuxAll0, x)), Head(Concept(E, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DisjointRange") {
    val input = Set(
      DisjointClassesAxiom(B, C),
      ObjectPropertyRangeAxiom(R, C),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x), Concept(C, x)), Head()),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: DisjointRangeSuper") {
    val input = Set(
      DisjointClassesAxiom(D, C),
      ObjectPropertyRangeAxiom(R, C),
      SubClassOfAxiom(A, E),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(B, D),
      SubClassOfAxiom(B, F),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(E, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(F, x))),
      OntologyClause(Body(Concept(D, x), Concept(C, x)), Head()),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Ticket465") {
    val input = Set(
      EquivalentClassesAxiom(D, ObjectIntersectionOf(ObjectSomeValuesFrom(T, E), A)),
      SubClassOfAxiom(B, A),
      SubClassOfAxiom(C, B),
      SubClassOfAxiom(F, C),
      SubClassOfAxiom(F, ObjectSomeValuesFrom(R, ObjectIntersectionOf(ObjectSomeValuesFrom(S, G), E))),
      SubObjectPropertyOfAxiom(R, T),
    )
    val expected = Set(
      OntologyClause(Body(Concept(B, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(E, f1))),
      OntologyClause(Body(Concept(D, x)), Head(Role(T, x, f1))),
      OntologyClause(Body(Concept(F, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(F, x)), Head(Concept(E, f2))),
      OntologyClause(Body(Concept(F, x)), Head(Concept(AuxSome1, f2))),
      OntologyClause(Body(Concept(F, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(AuxSome1, x)), Head(Concept(G, f3))),
      OntologyClause(Body(Concept(AuxSome1, x)), Head(Role(S, x, f3))),
      OntologyClause(Body(Role(AuxAll0, x, z1), Concept(E, x)), Head(Concept(D, z1))), // FIXME: this axiom is being repeated
      OntologyClause(Body(Role(T, x, z2), Concept(A, x)), Head(Role(AuxAll0, z2, x))),
      OntologyClause(Body(Role(R, x, z3), Concept(A, x)), Head(Role(AuxAll0, z3, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: NestedSubProperty") {
    val input = Set(
      EquivalentClassesAxiom(A, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(T, C))),
      EquivalentClassesAxiom(B, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(S, C))),
      SubObjectPropertyOfAxiom(T, S),
    )
    val expected = Set( // FIXME: expected output is wrong
      OntologyClause(Body(Concept(A, x)), Head(Concept(AuxSome0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(AuxSome1, f2))),
      OntologyClause(Body(Concept(B, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Concept(C, x), Role(T, z1, x)), Head(Role(AuxAll1, z1, x))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Concept(C, f3))),
      OntologyClause(Body(Concept(AuxSome0, x)), Head(Role(T, x, f3))),
      OntologyClause(Body(Concept(AuxSome1, x)), Head(Concept(C, f4))),
      OntologyClause(Body(Concept(AuxSome1, x)), Head(Role(S, x, f4))),
      OntologyClause(Body(Role(AuxAll0, x, z2), Role(R, z3, x)), Head(Concept(A, z3))),
      OntologyClause(Body(Role(AuxAll2, x, z4), Role(R, z5, x)), Head(Concept(B, z5))),
      OntologyClause(Body(Role(S, z6, x), Concept(AuxAll0, x)), Head(Role(AuxAll2, z6, x))),
      OntologyClause(Body(Role(T, z7, x)), Head(Role(S, z7, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: endocarditis and Heart") {
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
      SubObjectPropertyOfAxiom(ObjectPropertyChain(partOf, partOf), partOf),
      SubObjectPropertyOfAxiom(partOf, containedIn),
    )
    val expected = Set(
      OntologyClause(Body(Concept(Endocarditis, x)), Head(Concept(Endocardium, f1))),
      OntologyClause(Body(Concept(Endocarditis, x)), Head(Concept(Inflammation, x))),
      OntologyClause(Body(Concept(Endocarditis, x)), Head(Role(hasLocation, x, f1))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Concept(HeartValve, f2))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Concept(Tissue, x))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Role(containedIn, x, f2))),
      OntologyClause(Body(Concept(Heart, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Concept(HeartDisease, x)), Head(Concept(Disease, x))),
      OntologyClause(Body(Concept(HeartDisease, x)), Head(Concept(Heart, f3))),
      OntologyClause(Body(Concept(HeartDisease, x)), Head(Role(hasLocation, x, f3))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Concept(AuxAll1, x))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Concept(BodyValve, x))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Concept(Heart, f4))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Role(partOf, x, f4))),
      OntologyClause(Body(Concept(Inflammation, x)), Head(Concept(Disease, x))),
      OntologyClause(Body(Concept(Inflammation, x)), Head(Concept(Tissue, f5))),
      OntologyClause(Body(Concept(Inflammation, x)), Head(Role(actsOn, x, f5))),
      OntologyClause(Body(Concept(Pericarditis, x)), Head(Concept(Inflammation, x))),
      OntologyClause(Body(Concept(Pericarditis, x)), Head(Concept(Pericardium, f6))),
      OntologyClause(Body(Concept(Pericarditis, x)), Head(Role(hasLocation, x, f6))),
      OntologyClause(Body(Concept(Pericardium, x)), Head(Concept(Heart, f7))),
      OntologyClause(Body(Concept(Pericardium, x)), Head(Concept(Tissue, x))),
      OntologyClause(Body(Concept(Pericardium, x)), Head(Role(containedIn, x, f7))),
      OntologyClause(Body(Role(AuxAll2, x, z1), Concept(AuxAll0, x)), Head(Concept(HeartDisease, z1))),
      OntologyClause(Body(Role(AuxAll3, x, z2), Concept(AuxAll1, x)), Head(Concept(IsStateNeedsTreatment, z2))),
      OntologyClause(Body(Role(containedIn, z3, x), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z3))), // FIXME: this axiom should not be repeated
      OntologyClause(Body(Role(containedIn, z4, x), Concept(AuxAll1, x)), Head(Concept(AuxAll1, z4))),
      OntologyClause(Body(Role(hasLocation, x, z5), Concept(Disease, x)), Head(Role(AuxAll2, z5, x))),
      OntologyClause(Body(Role(hasLocation, x, z6), Concept(HeartDisease, x)), Head(Role(AuxAll3, z6, x))),
      OntologyClause(Body(Role(partOf, z7, x)), Head(Role(containedIn, z7, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
   // assert(decorate(actual) === decorate(expected))
  }

  test("corresponds to: Heart") {
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
      SubObjectPropertyOfAxiom(partOf, containedIn),
    )
    val expected = Set(
      OntologyClause(Body(Concept(Endocarditis, x)), Head(Concept(Endocardium, f1))),
      OntologyClause(Body(Concept(Endocarditis, x)), Head(Concept(Inflammation, x))),
      OntologyClause(Body(Concept(Endocarditis, x)), Head(Role(hasLocation, x, f1))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Concept(HeartValve, f2))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Concept(HeartWall, f3))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Concept(Tissue, x))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Role(containedIn, x, f3))),
      OntologyClause(Body(Concept(Endocardium, x)), Head(Role(containedIn, x, f2))),
      OntologyClause(Body(Concept(Heart, x)), Head(Concept(AuxAll0, x))),
      OntologyClause(Body(Concept(HeartDisease, x)), Head(Concept(Disease, x))),
      OntologyClause(Body(Concept(HeartDisease, x)), Head(Concept(Heart, f4))),
      OntologyClause(Body(Concept(HeartDisease, x)), Head(Role(hasLocation, x, f4))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Concept(AuxAll1, x))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Concept(BodyValve, x))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Concept(Heart, f5))),
      OntologyClause(Body(Concept(HeartValve, x)), Head(Role(partOf, x, f5))),
      OntologyClause(Body(Concept(HeartWall, x)), Head(Concept(BodyWall, x))),
      OntologyClause(Body(Concept(HeartWall, x)), Head(Concept(Heart, f6))),
      OntologyClause(Body(Concept(HeartWall, x)), Head(Role(partOf, x, f6))),
      OntologyClause(Body(Concept(Inflammation, x)), Head(Concept(Disease, x))),
      OntologyClause(Body(Concept(Inflammation, x)), Head(Concept(Tissue, f7))),
      OntologyClause(Body(Concept(Inflammation, x)), Head(Role(actsOn, x, f7))),
      OntologyClause(Body(Role(AuxAll2, x, z1), Concept(AuxAll0, x)), Head(Concept(HeartDisease, z1))),
      OntologyClause(Body(Role(AuxAll3, x, z2), Concept(AuxAll1, x)), Head(Concept(CriticalDisease, z2))),
      OntologyClause(Body(Role(containedIn, z3, x), Concept(AuxAll0, x)), Head(Concept(AuxAll0, z3))),
      OntologyClause(Body(Role(containedIn, z4, x), Concept(AuxAll1, x)), Head(Concept(AuxAll1, z4))),
      OntologyClause(Body(Role(hasLocation, x, z5), Concept(Disease, x)), Head(Role(AuxAll2, z5, x))),
      OntologyClause(Body(Role(hasLocation, x, z6), Concept(HeartDisease, x)), Head(Role(AuxAll3, z6, x))),
      OntologyClause(Body(Role(partOf, z7, x)), Head(Role(containedIn, z7, x))),
    )
    val actual = transform(input)
    assert(actual forall isHorn)
    //assert(decorate(actual) === decorate(expected))
  }

}