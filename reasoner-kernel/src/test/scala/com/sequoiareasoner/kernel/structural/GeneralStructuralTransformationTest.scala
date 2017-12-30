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
import org.scalatest.FunSuite


/** The purpose of this test is to test the structural transformation one GCI at a time, and to try all possible
  * combinations of constructors to get good test coverage.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class GeneralStructuralTransformationTest extends FunSuite {
  import ClauseSetEquivalenceUtilities._
  import CommonNames._
  import OWLAxiomBuilder._

  // Passes the current suite implicitly to the decorated sets.
  private[this] implicit val self = this

  // Dummy ontology to allow for the construction of concepts and roles.
  private[this] implicit val dlOntology: DLOntology = null

  private[this] val T0 = IRI.all() // TODO: Use the correct kind of auxiliary predicate name (either some, all, disjunction)
  private[this] val T1 = IRI.all()
  private[this] val T2 = IRI.all()
  private[this] val T3 = IRI.all()

  // FIXME: test for several universals or existentials on the left

  test("A Or B -> C And D") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(D, x))),
    )
    val gci = SubClassOfAxiom(ObjectUnionOf(A, B), ObjectIntersectionOf(C, D))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A And B -> C Or D") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head(Concept(C, x), Concept(D, x))),
    )
    val gci = SubClassOfAxiom(ObjectIntersectionOf(A, B), ObjectUnionOf(C, D))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> ForAll R.(Not(B) Or Not(C) Or (A And B))") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(T0, z1))),
      OntologyClause(Body(Concept(B, x), Concept(C, x), Concept(T0, x)), Head(Concept(A, x))),
      OntologyClause(Body(Concept(B, x), Concept(C, x), Concept(T0, x)), Head(Concept(B, x))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(R, ObjectUnionOf(ObjectComplementOf(B), ObjectComplementOf(C), ObjectIntersectionOf(A, B))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("C And D -> ForAll R.B Or ForAll R.C") {
    val expected = Set(
      OntologyClause(Body(Concept(C, x), Concept(D, x), Role(R, x, z1), Role(R, x, z2)), Head(Concept(B, z1), Concept(C, z2))),
    )
    val gci = SubClassOfAxiom(ObjectIntersectionOf(C, D), ObjectUnionOf(ObjectAllValuesFrom(R, B), ObjectAllValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Exists R.C -> B Or C") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(B, z1), Concept(C, z1))),
    )
    val gci = SubClassOfAxiom(ObjectSomeValuesFrom(R, C), ObjectUnionOf(B, C))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Exists R.C And Exists S.D -> B Or C") {
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Concept(C, x)), Head(Concept(T0, z1))),
      OntologyClause(Body(Role(S, z2, x), Concept(D, x)), Head(Concept(T1, z2))),
      OntologyClause(Body(Concept(T0, x), Concept(T1, x)), Head(Concept(B, x), Concept(C, x))),
    )
    val gci = SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(S, D)), ObjectUnionOf(B, C))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Exists R.C And Exists S.D -> ForAll R.B") {
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(T1, x), Role(R, x, z1)), Head(Concept(B, z1))),
      OntologyClause(Body(Role(R, z2, x), Concept(C, x)), Head(Concept(T0, z2))),
      OntologyClause(Body(Role(S, z3, x), Concept(D, x)), Head(Concept(T1, z3))),
    )
    val gci = SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(S, D)), ObjectAllValuesFrom(R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Exists R.C And Exists S.D -> ForAll R.B Or ForAll R.C") {
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(R, x, z2), Concept(T0, x), Concept(T3, x)), Head(Concept(B, z1), Concept(C, z2))),
      OntologyClause(Body(Role(R, z3, x), Concept(C, x)), Head(Concept(T0, z3))),
      OntologyClause(Body(Role(S, z4, x), Concept(D, x)), Head(Concept(T3, z4))),
    )
    val gci = SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(S, D)), ObjectUnionOf(ObjectAllValuesFrom(R, B), ObjectAllValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Exists R.(Exists R.X) And Exists S.D -> ForAll R.B Or ForAll R.C") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(T1, x), Role(R, x, z1), Role(R, x, z2)), Head(Concept(B, z1), Concept(C, z2))),
      OntologyClause(Body(Role(R, z3, x), Concept(X, x)), Head(Concept(T2, z3))),
      OntologyClause(Body(Role(R, z4, x), Concept(T2, x)), Head(Concept(T1, z4))),
      OntologyClause(Body(Role(S, z5, x), Concept(D, x)), Head(Concept(T0, z5))),
    )
    val gci = SubClassOfAxiom(ObjectIntersectionOf(ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, X)), ObjectSomeValuesFrom(S, D)), ObjectUnionOf(ObjectAllValuesFrom(R, B), ObjectAllValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll R.B") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(B, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Exists R.B") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f3))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll R.(ForAll R.C)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(T1, z1))),
      OntologyClause(Body(Role(R, x, z2), Concept(T1, x)), Head(Concept(C, z2))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(R, ObjectAllValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Exists R.(Exists R.C)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f2))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll R.(Exists R.C)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(T0, z1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(R, ObjectSomeValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Exists R.(ForAll R.C)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f4))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f4))),
      OntologyClause(Body(Role(R, x, z1), Concept(T0, x)), Head(Concept(C, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectAllValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Exists R.Not(B)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f4))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f4))),
      OntologyClause(Body(Concept(B, x), Concept(T0, x)), Head()),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(R, ObjectComplementOf(B)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Exists R.(Exists R.X) -> ForAll R.B Or ForAll R.C") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Concept(T1, x)), Head(Concept(T2, z1))),
      OntologyClause(Body(Role(R, z2, x), Concept(X, x)), Head(Concept(T1, z2))),
      OntologyClause(Body(Role(R, x, z3), Role(R, x, z4), Concept(T2, x)), Head(Concept(B, z3), Concept(C, z4))),
    )
    val gci = SubClassOfAxiom(ObjectSomeValuesFrom(R, ObjectSomeValuesFrom(R, X)), ObjectUnionOf(ObjectAllValuesFrom(R, B), ObjectAllValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Not(B)"){
    val expected = Set(OntologyClause(Body(Concept(A, x), Concept(B, x)), Head()))
    val gci = SubClassOfAxiom(A, ObjectComplementOf(B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Not(Not(Not(B)))"){
    // Expected output should be the same as the previous test
    val expected = Set(OntologyClause(Body(Concept(A, x), Concept(B, x)), Head()))
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectComplementOf(ObjectComplementOf(B))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Not(Not(B))"){
    val expected = Set(OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))))
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectComplementOf(B)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Not(Not(Not(Not(B))))"){
    // Expected output should be the same as the previous test
    val expected = Set(OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))))
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectComplementOf(ObjectComplementOf(ObjectComplementOf(B)))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> ForAll S.(ForAll R.C And ForAll R.D)") {
    // Tests that the fillers of the two universals are conjoined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Role(R, x, z3)), Head(Concept(D, z3))),
      OntologyClause(Body(Concept(T0, x), Role(R, x, z2)), Head(Concept(C, z2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectAllValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll S.(Exists R.C And Exists R.D)") {
    val expected = Set(
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(D, f2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll S.(ForAll R.C Or ForAll R.D)") {
    // Tests that the two inner universals are not accidentally combined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Role(R, x, z2), Role(R, x, z3)), Head(Concept(C, z2), Concept(D, z3))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectUnionOf(ObjectAllValuesFrom(R, C), ObjectAllValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll S.(Exists R.C Or Exists R.D)") {
    // Tests that the fillers of the two universals are conjoined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(T1, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Role(S, x, z1), Concept(A, x)), Head(Concept(T2, z1), Concept(T1, z1))),
      OntologyClause(Body(Concept(T2, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(T2, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(T1, x)), Head(Concept(D, f2))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectUnionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Not(Exists R.Not(C))") {
    // Tests that negation normal form is implemented correctly
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(R, ObjectComplementOf(C))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Not(ForAll R.C)") {
    // Tests that negation normal form is implemented correctly
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(T0, x), Concept(C, x)), Head()),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectAllValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Not(ForAll R.Not(C))") {
    // Tests that negation normal form is implemented correctly
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectAllValuesFrom(R, ObjectComplementOf(C))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll S.(Exists R.C)") {
    val expected = Set(
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectSomeValuesFrom(R, C)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> ForAll S.Not(ForAll R.Not(C))") {
    // The output of this test should be the same as the previous case (negations pushed inwards)
    val expected = Set(
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectComplementOf(ObjectAllValuesFrom(R, ObjectComplementOf(C)))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> ForAll S.(Exists R.C Or ForAll S.D)") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Role(S, x, z3)), Head(Concept(C, f1), Concept(D, z3))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Role(R, x, f1), Concept(D, z2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectUnionOf(ObjectSomeValuesFrom(R, C), ObjectAllValuesFrom(S, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> ForAll S.Not(ForAll R.Not(C) And Exists S.Not(D))") { // TODO: update expected output
    // The output of this test should be the same as the previous case (negations pushed inwards)
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Role(S, x, z3)), Head(Concept(C, f1), Concept(D, z3))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Role(R, x, f1), Concept(D, z2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectComplementOf(C)), ObjectSomeValuesFrom(S, ObjectComplementOf(D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> ForAll S.Not(ForAll R.C And Exists S.D)") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Concept(T1, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(T2, x), Concept(T3, x)), Head(Concept(T1, f1))),
      OntologyClause(Body(Concept(T2, x), Concept(T3, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(S, z2, x), Concept(D, x)), Head(Concept(T3, z2))),
      OntologyClause(Body(Role(S, x, z1), Concept(A, x)), Head(Concept(T2, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D)))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(ForAll S.(ForAll R.C And Exists S.D))") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(S, z1, x), Concept(D, x)), Head(Concept(T1, z1))),
      OntologyClause(Body(Concept(T0, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(T1, x), Concept(T2, x)), Head(Concept(T0, f2))),
      OntologyClause(Body(Concept(T1, x), Concept(T2, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(T2, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectAllValuesFrom(S, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D)))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(ForAll S.Not(ForAll R.C AND Exists S.D))") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(D, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(S, x, f2))),
      OntologyClause(Body(Concept(T0, x), Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectAllValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(ForAll R.C And ForAll R.D))") {
    // Tests that the fillers of the two universals are conjoined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(T0, x, z2)), Head(Concept(C, z1))),
      OntologyClause(Body(Role(R, x, z3), Role(T0, x, z4)), Head(Concept(D, z3))),
      OntologyClause(Body(Role(S, x, z5), Concept(A, x)), Head(Role(T0, z5, x))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectAllValuesFrom(R, D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(Exists R.C And Exists R.D))") {
    // FIXME: clause set optimizer does not yet eliminate T1
    val expected = Set(
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f1))),
      OntologyClause(Body(Concept(T1, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(T1, x)), Head(Concept(D, f2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T1, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(ForAll R.C))") {
    // Tests that the two inner universals are not accidentally combined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(T0, x, z2)), Head(Concept(C, z1))),
      OntologyClause(Body(Role(S, x, z3), Concept(A, x)), Head(Role(T0, z3, x))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectAllValuesFrom(R, C)))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  // TODO: all test cases above this line were hand verified to be correct on 25/05/2015

  ignore("A -> Not(Exists S.Not(ForAll R.C Or ForAll R.D))") {
    // Tests that the two inner universals are not accidentally combined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Role(R, x, z2), Role(R, x, z3)), Head(Concept(C, z2), Concept(D, z3))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectUnionOf(ObjectAllValuesFrom(R, C), ObjectAllValuesFrom(R, D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(Exists R.C Or Exists R.D))") {
    // Tests that the fillers of the two universals are conjoined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f1), Concept(D, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f1), Concept(D, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f1), Role(R, x, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f1), Role(R, x, f2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectUnionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(Exists R.C Or ForAll S.D))") {
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Concept(C, f1), Concept(D, z2))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Role(R, x, f1), Concept(D, z2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectUnionOf(ObjectSomeValuesFrom(R, C), ObjectAllValuesFrom(S, D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(Not(ForAll R.Not(C) AND Exists S.Not(D))))") {
    // The output of this test should be the same as the pervious case (negations pushed inwards)
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Concept(C, f1), Concept(D, z2))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Role(R, x, f1), Concept(D, z2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectComplementOf(C)), ObjectSomeValuesFrom(S, ObjectComplementOf(D))))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(Not(ForAll R.C AND Exists S.D)))") {
    val expected = Set(
      OntologyClause(Body(Concept(T2, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(T1, x), Concept(D, x)), Head()),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Concept(T2, f1), Concept(T1, z2))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Role(R, x, f1), Concept(T1, z2))),
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D)))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Not(Exists S.Not(ForAll R.C AND Exists S.D)))") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Concept(T1, f2), Concept(T2, z1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Role(R, x, f2), Concept(T2, z1))),
      OntologyClause(Body(Concept(T1, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(T2, x), Concept(D, x)), Head()),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D)))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Not(Exists S.Not(Not(ForAll R.C AND Exists S.D))))") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(D, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(S, x, f2))),
      OntologyClause(Body(Concept(T0, x), Role(R, x, z1)), Head(Concept(C, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D))))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Exists S.(ForAll R.C And ForAll R.D)") {
    // Tests that the fillers of the two universals are conjoined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(T0, x), Role(R, x, z2)), Head(Concept(D, z2))),
      OntologyClause(Body(Concept(T0, x), Role(R, x, z1)), Head(Concept(C, z1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectAllValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Exists S.(Exists R.C And Exists R.D)") {
    // Tests that the fillers of the two universals are conjoined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(D, f3))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f3))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectIntersectionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("A -> Exists S.(ForAll R.C Or ForAll R.D)") {
    // Tests that the two inner universals are not accidentally combined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(A,x)), Head(Concept(T0,f1))),
      OntologyClause(Body(Concept(A,x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0,x), Role(R, x, z1), Role(R, x,z2)), Head(Concept(C,z1), Concept(D,z2))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectUnionOf(ObjectAllValuesFrom(R, C), ObjectAllValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Exists S.(Exists R.C Or Exists R.D)") {
    // Tests that the fillers of the two universals are conjoined into a single ForAll
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f2), Concept(D, f3))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f2), Concept(D, f3))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(C, f2), Role(R, x, f3))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(R, x, f2), Role(R, x, f3))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectUnionOf(ObjectSomeValuesFrom(R, C), ObjectSomeValuesFrom(R, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Exists S.(Exists R.C Or ForAll S.D)") { // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Concept(C, f2), Concept(D, z1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Role(R, x, f2), Concept(D, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectUnionOf(ObjectSomeValuesFrom(R, C), ObjectAllValuesFrom(S, D))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Exists S.Not(ForAll R.Not(C) AND Exists S.Not(D))") { // TODO: update expected output
    // The output of this test should be the same as the previous case (negations pushed inwards)
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Concept(C, f2), Concept(D, z1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Role(R, x, f2), Concept(D, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, ObjectComplementOf(C)), ObjectSomeValuesFrom(S, ObjectComplementOf(D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Exists S.Not(ForAll R.C AND Exists S.D)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T1, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(T2, x), Concept(D, x)), Head()),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Concept(T1, f2), Concept(T2, z1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z1)), Head(Role(R, x, f2), Concept(T2, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D)))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.(ForAll R.C AND Exists S.D))") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Concept(T1, f1), Concept(T2, z2))),
      OntologyClause(Body(Concept(T0, x), Role(S, x, z2)), Head(Role(R, x, f1), Concept(T2, z2))),
      OntologyClause(Body(Concept(T1, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(T2, x), Concept(D, x)), Head()),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D)))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> Not(Exists S.Not(ForAll R.C AND Exists S.D))") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(S, x, z1)), Head(Concept(T0, z1))),
      OntologyClause(Body(Concept(T0, x)), Head(Concept(D, f1))),
      OntologyClause(Body(Concept(T0, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Concept(T0, x), Role(R, x, z2)), Head(Concept(C, z2))),
    )
    val gci = SubClassOfAxiom(A, ObjectComplementOf(ObjectSomeValuesFrom(S, ObjectComplementOf(ObjectIntersectionOf(ObjectAllValuesFrom(R, C), ObjectSomeValuesFrom(S, D))))))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("ObjectPropertyDomain(R A)") { // TODO: add more complex tests
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, x))),
    )
    val gci = ObjectPropertyDomainAxiom(R, A)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("ObjectPropertyRange(R A)") { // TODO: add more complex tests
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, z1))),
    )
    val gci = ObjectPropertyRangeAxiom(R, A)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("DisjointClasses(A A)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head()),
    )
    val gci = DisjointClassesAxiom(A, A)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("DisjointClasses(A B C)") { // TODO: add more complex tests
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Concept(C, x)), Head()),
      OntologyClause(Body(Concept(B, x), Concept(C, x)), Head()),
    )
    val gci = DisjointClassesAxiom(A, B, C)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (>= 2 R. B)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectMinCardinality(2, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (>= 2 R. Not(B))") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(T0, f2))),
      OntologyClause(Body(Concept(T0, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectMinCardinality(2, R, ObjectComplementOf(B)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (>= 2 R. B) Or C") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1), Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f2), Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1), Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f2), Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1), Concept(C, x))),
    )
    val gci = SubClassOfAxiom(A, ObjectUnionOf(ObjectMinCardinality(2, R, B), C))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (>= 2 R. B) And C") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectIntersectionOf(ObjectMinCardinality(2, R, B), C))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (>= 4 R. B)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f4))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f3))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f4))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f3, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f4, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f3, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f4, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f4, f3))),
    )
    val gci = SubClassOfAxiom(A, ObjectMinCardinality(4, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (<= 2 R. B)") {
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1), Role(R, x, z2), Role(R, x, z3)), Head(Concept(T0, z1), Concept(T0, z2), Concept(T0, z3), Equality(z1, z2), Equality(z1, z3), Equality(z2, z3))),
    )
    val gci = SubClassOfAxiom(A, ObjectMaxCardinality(2, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (<= 2 R. Not(B))") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x), Role(R, x, z1), Role(R, x, z2), Role(R, x, z3)), Head(Concept(B, z1), Concept(B, z2), Concept(B, z3), Equality(z1, z2), Equality(z1, z3), Equality(z2, z3))),
    )
    val gci = SubClassOfAxiom(A, ObjectMaxCardinality(2, R, ObjectComplementOf(B)))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (== 2 R. B)") {
    // This is the union of the sets of clauses given by A -> (>= 2 R. B) and A -> (<= 2 R. B)
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1), Role(R, x, z2), Role(R, x, z3)), Head(Concept(T0, z1), Concept(T0, z2), Concept(T0, z3), Equality(z1, z2), Equality(z1, z3), Equality(z2, z3))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f2))),
      OntologyClause(Body(Concept(A, x)), Head(Inequality(f2, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectExactCardinality(2, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (>= 1 R. B)") {
    // This should be the same as an existential on in the head
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectMinCardinality(1, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (<= 1 R. B)") {
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1), Role(R, x, z2)), Head(Concept(T0, z1), Concept(T0, z2), Equality(z1, z2)))

    )
    val gci = SubClassOfAxiom(A, ObjectMaxCardinality(1, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (<= 0 R. B)") {
    // This is the union of the sets of clauses given by A -> (>= 1 R. B) and A -> (<= 1 R. B)
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectMaxCardinality(0, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (>= 0 R. B)") {
    // This is the union of the sets of clauses given by A -> (>= 1 R. B) and A -> (<= 1 R. B)
    val expected = Set(
      OntologyClause(Body(Concept(A,x)), Head(Concept(Thing, x))),
    )
    val gci = SubClassOfAxiom(A, ObjectMinCardinality(0, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (== 0 R. B)") {
    // This should be the same as the case of A -> (<= 0 R. B)
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1)), Head(Concept(T0, z1))),
    )
    val gci = SubClassOfAxiom(A, ObjectExactCardinality(0, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("A -> (== 1 R. B)") {
    // This is the union of the sets of clauses given by A -> (>= 1 R. B) and A -> (<= 1 R. B)
    val expected = Set(
      OntologyClause(Body(Concept(T0, x), Concept(B, x)), Head()),
      OntologyClause(Body(Concept(A, x), Role(R, x, z1), Role(R, x, z2)), Head(Concept(T0, z1), Concept(T0, z2), Equality(z1, z2))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, f1))),
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectExactCardinality(1, R, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("SubObjectPropertyOf(R S)") {
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Role(S, x, z1))),
    )
    val gci = SubObjectPropertyOfAxiom(R, S)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("EquivalentClasses(A B)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(A, x))),
    )
    val gci = EquivalentClassesAxiom(A, B)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("EquivalentClasses(A B C)") { // TODO: add more complex tests
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(A, x))),
    )
    val gci = EquivalentClassesAxiom(A, B, C)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("EquivalentClasses(A B C D)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(B, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(D, x)), Head(Concept(A, x))),
    )
    val gci = EquivalentClassesAxiom(A, B, C, D)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  /*
      Test to add:

      DisjointClassesAxiom(ObjectSomeValuesFrom(R, C), D),
      SubClassOfAxiom(A, D),
      SubClassOfAxiom(A, ObjectSomeValuesFrom(R, B)),
      SubClassOfAxiom(ObjectSomeValuesFrom(R, B), ObjectSomeValuesFrom(R, C))

      should be clausified into:

      OntologyClause(Concept(A,x) -> Concept(D,x))
      OntologyClause(Concept(A,x) -> Concept(B,f1(x)))
      OntologyClause(Concept(A,x) -> Role(R,x,f1(x)))
      OntologyClause(Role(R,z_1,x) AND Concept(C,x) -> Role(T1,z_1,x))
      OntologyClause(Role(T1,x,z_1) AND Concept(D,x) -> )
      OntologyClause(Role(R,z_1,x) AND Concept(B,x) -> Role(T4,z_1,x))
      OntologyClause(Role(T4,x,z_1) -> Concept(C,f2(x)))
      OntologyClause(Role(T4,x,z_1) -> Role(R,x,f2(x)))
   */

  // Test to add: SubClassOfAxiom(ObjectIntersectionOf(ObjectAllValuesFrom(R, A), ObjectAllValuesFrom(Q, B)), X)

  ignore("Range(R, A)"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(A, x))),
    )
    val gci = ObjectPropertyRangeAxiom(R, A)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Range(R, Exists S.A)"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x)), Head(Role(S, x, f1))),
      OntologyClause(Body(Role(R, z2, x)), Head(Concept(A, f1))),
    )
    val gci = ObjectPropertyRangeAxiom(R, ObjectSomeValuesFrom(S, A))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Range(R, ForAll S.A)"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Role(S, x, z2)), Head(Concept(A, z2))),
    )
    val gci = ObjectPropertyRangeAxiom(R, ObjectAllValuesFrom(S, A))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Property range with conjunction"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(A, x))),
      OntologyClause(Body(Role(R, z2, x)), Head(Concept(B, x))),
    )
    val gci = ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(A, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Property range with disjunction"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x)), Head(Concept(A, x), Concept(B, x))),
    )
    val gci = ObjectPropertyRangeAxiom(R, ObjectUnionOf(A, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Property range with negated atomic"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Concept(A, x)), Head()),
    )
    val gci = ObjectPropertyRangeAxiom(R, ObjectComplementOf(A))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Property range with disjunction (positive and negative)"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Concept(A, x)), Head(Concept(B, x))),
    )
    val gci = ObjectPropertyRangeAxiom(R, ObjectUnionOf(ObjectComplementOf(A), B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Property range with conjunction (positive and negative)"){ // TODO: update expected output
    val expected = Set(
      OntologyClause(Body(Role(R, z1, x), Concept(A, x)), Head()),
      OntologyClause(Body(Role(R, z2, x)), Head(Concept(B, x))),
    )
    val gci = ObjectPropertyRangeAxiom(R, ObjectIntersectionOf(ObjectComplementOf(A), B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with atomic concept"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, x))),
    )
    val gci = ObjectPropertyDomainAxiom(R, A)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with existential"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Role(S, x, f1))),
      OntologyClause(Body(Role(R, x, z2)), Head(Concept(A, f1))),
    )
    val gci = ObjectPropertyDomainAxiom(R, ObjectSomeValuesFrom(S, A))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with universal"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Role(S, x, z2)), Head(Concept(A, z2))),
    )
    val gci = ObjectPropertyDomainAxiom(R, ObjectAllValuesFrom(S, A))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with conjunction"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, x))),
      OntologyClause(Body(Role(R, x, z2)), Head(Concept(B, x))),
    )
    val gci = ObjectPropertyDomainAxiom(R, ObjectIntersectionOf(A, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with disjunction"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(A, x), Concept(B, x))),
    )
    val gci = ObjectPropertyDomainAxiom(R, ObjectUnionOf(A, B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with negated atomic"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Concept(A, x)), Head()),
    )
    val gci = ObjectPropertyDomainAxiom(R, ObjectComplementOf(A))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with disjunction (positive and negative)"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Concept(A, x)), Head(Concept(B, x))),
    )
    val gci = ObjectPropertyDomainAxiom(R, ObjectUnionOf(ObjectComplementOf(A), B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Property domain with conjunction (positive and negative)"){
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1), Concept(A, x)), Head()),
      OntologyClause(Body(Role(R, x, z2)), Head(Concept(B, x))),
    )
    val gci = ObjectPropertyDomainAxiom(R, ObjectIntersectionOf(ObjectComplementOf(A), B))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Thing in body (atomic)") {
    val expected = Set(
      OntologyClause(Body(), Head(Concept(B, x))),
    )
    val gci = SubClassOfAxiom(Thing, B)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Thing in body (conjunction)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
    )
    val gci = SubClassOfAxiom(ObjectIntersectionOf(A, Thing), B)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Eliminate owl:Thing in body (disjunction)") { // FIXME: no subsumption testing for input clauses
    val expected = Set(
      OntologyClause(Body(), Head(Concept(B, x))),
    )
    val gci = SubClassOfAxiom(ObjectUnionOf(A, Thing), B)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Eliminate owl:Thing in body (existential)") {
    val expected = Set(
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(B, x))),
    )
    val gci = SubClassOfAxiom(ObjectSomeValuesFrom(R, Thing), B)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("Eliminate owl:Thing in body (universal)") {
    val expected = Set(
      OntologyClause(Body(), Head(Concept(B, f1))),
      OntologyClause(Body(), Head(Role(R, f1, x))),
    )
    val gci = SubClassOfAxiom(ObjectAllValuesFrom(R, Thing), B)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Thing in head (atomic)") {
    val expected = Set.empty[OntologyClause]
    val gci = SubClassOfAxiom(A, Thing)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Thing in head (conjunction)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
    )
    val gci = SubClassOfAxiom(A, ObjectIntersectionOf(B, Thing))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Thing in head (disjunction)") {
    val expected = Set.empty[OntologyClause]
    val gci = SubClassOfAxiom(A, ObjectUnionOf(B, Thing))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Thing in head (existential)") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
    )
    val gci = SubClassOfAxiom(A, ObjectSomeValuesFrom(R, Thing))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Thing in head (universal)") {
    val expected = Set.empty[OntologyClause]
    val gci = SubClassOfAxiom(A, ObjectAllValuesFrom(R, Thing))
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Nothing in body") {
    val expected = Set.empty[OntologyClause]
    val gci = SubClassOfAxiom(ObjectIntersectionOf(A, Nothing), B)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  test("Eliminate owl:Nothing in head") {
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head()),
    )
    val gci = SubClassOfAxiom(A, Nothing)
    val actual = transform(gci)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("No duplicate internal IRIs") { // TODO: check what happens when two cases are the same only after NNF
    /* We are looking to check that ObjectSomeValuesFrom(R, X) occurring on the left in *two* different places only
     * results in *one* axiom of the form
     *   OntologyClause(Concept(X, x) AND Role(R, z, x) -> Concept(T0, z))
     * and not two.
     */
    val input = Set(
      EquivalentClassesAxiom(ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, X)), B),
      EquivalentClassesAxiom(ObjectIntersectionOf(C, ObjectSomeValuesFrom(R, X)), D),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

  ignore("No duplicate internal IRIs after NNF") { // TODO: check what happens when two cases are the same only after NNF
    /* We are looking to check that ObjectSomeValuesFrom(R, X) occurring on the left in *two* different places only
     * results in *one* axiom of the form
     *   OntologyClause(Concept(X, x) AND Role(R, z, x) -> Concept(T0, z))
     * and not two.
     */
    val input = Set(
      SubClassOfAxiom(ObjectIntersectionOf(A, ObjectSomeValuesFrom(R, ObjectComplementOf(B))), Nothing),
      SubClassOfAxiom(A, ObjectAllValuesFrom(R, B)),
    )
    val expected = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
    )
    val actual = transform(input)
    assert(decorate(actual) === decorate(expected))
  }

}
