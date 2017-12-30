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
import rationals.properties.ModelCheck

class AutomatonBuilderTest extends FunSuite {
  import AutomatonBuilder._
  import CommonNames._
  import OWLAxiomBuilder._

  private[this] def convert(s: Set[_ <: SubObjectPropertyOfAxiom[SubObjectPropertyExpression]]): Set[RoleInclusionAxiom] =
    s collect {
      case SubObjectPropertyOfAxiom(ObjectPropertyChain(chain), rhs) =>
        RoleInclusionAxiom(chain, rhs)
      case SubObjectPropertyOfAxiom(lhs: ObjectPropertyExpression, rhs) =>
        RoleInclusionAxiom(Seq(lhs), rhs)
    }

  private[this] def assertEquiv(actual: NFA, expected: NFA): Unit = {
    val mc = new ModelCheck[ObjectPropertyExpression, rationals.Transition[ObjectPropertyExpression], Builder]
    // Check for language containment in both directions.
    assert(mc.test(actual, expected) && mc.test(expected, actual), {
      val epsilonRemover = new EpsilonTransitionRemover
      s"\nExpected: $expected\nActual: $actual\nEpsilon removed: ${epsilonRemover.transform(actual)}."
    })
  }

  private[this] def simpleSymmetryTest(set: Set[_ <: SubObjectPropertyOfAxiom[SubObjectPropertyExpression]])(op: ObjectProperty): Boolean = {
    set exists {
      case SubObjectPropertyOfAxiom(ope1: ObjectPropertyExpression, ope2: ObjectPropertyExpression) =>
        (ope1 == op && ope2 == op.inverse) || (ope1 == op.inverse && ope2 == op)
      case _ => false
    }
  }

  test("automaton 1"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2, S), Q),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R3, R4), S)
    )

    val expectedForQ = new NFA
    val initialStateForQ = expectedForQ.addState(true, false)
    val finalStateForQ = expectedForQ.addState(false, true)
    val s1 = expectedForQ.addState(false, false)
    val s3 = expectedForQ.addState(false, false)
    val s4 = expectedForQ.addState(false, false)
    expectedForQ.addTransition(Transition(initialStateForQ, Q,  finalStateForQ))
    expectedForQ.addTransition(Transition(initialStateForQ, R1, s1))
    expectedForQ.addTransition(Transition(s1, R2, s3))
    expectedForQ.addTransition(Transition(s3, S,  finalStateForQ))
    expectedForQ.addTransition(Transition(s3, R3, s4))
    expectedForQ.addTransition(Transition(s4, R4, finalStateForQ))

    val builderForQ = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForQ = builderForQ.getAutomaton(Q)
    assertEquiv(actualForQ, expectedForQ)

    val expectedForS = new NFA
    val initialStateForS = expectedForS.addState(true, false)
    val finalStateForS = expectedForS.addState(false, true)
    val t1 = expectedForS.addState(false, false)
    expectedForS.addTransition(Transition(initialStateForS, S,  finalStateForS))
    expectedForS.addTransition(Transition(initialStateForS, R3, t1))
    expectedForS.addTransition(Transition(t1, R4, finalStateForS))

    val builderForS = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForS = builderForS.getAutomaton(S)
    assertEquiv(actualForS, expectedForS)
  }

  test("automaton 2"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R),
      SubObjectPropertyOfAxiom(S, R)
    )
    val expectedForR = new NFA
    val initialState = expectedForR.addState(true, false)
    val finalState = expectedForR.addState(false, true)
    expectedForR.addTransition(Transition(initialState, R, finalState))
    expectedForR.addTransition(Transition(initialState, S, finalState))
    expectedForR.addTransition(Transition(finalState, R, finalState))
    expectedForR.addTransition(Transition(finalState, S, finalState))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForR = builder.getAutomaton(R)
    assertEquiv(actualForR, expectedForR)
  }

  test("automaton 3"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P)
    )
    val expectedForP = new NFA
    val initialState = expectedForP.addState(true, false)
    val finalState = expectedForP.addState(false, true)
    expectedForP.addTransition(Transition(initialState, P, finalState))
    expectedForP.addTransition(Transition(finalState, S, finalState))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForP = builder.getAutomaton(P)
    assertEquiv(actualForP, expectedForP)
  }

  test("automaton 4"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), R)
    )
    val expectedForR = new NFA
    val initialState = expectedForR.addState(true, false)
    val finalState = expectedForR.addState(false, true)
    val s1 = expectedForR.addState(false, false)
    expectedForR.addTransition(Transition(initialState, R, finalState))
    expectedForR.addTransition(Transition(initialState, P, s1))
    expectedForR.addTransition(Transition(s1, S, s1))
    expectedForR.addTransition(Transition(s1, S, finalState))

    val builderForR = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForR = builderForR.getAutomaton(R)
    assertEquiv(actualForR, expectedForR)
  }

  test("automaton 5"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S, R), P)
    )
    val expectedForP = new NFA
    val initialState = expectedForP.addState(true, false)
    val finalState = expectedForP.addState(false, true)
    val s1 = expectedForP.addState(false, false)
    expectedForP.addTransition(Transition(initialState, P, finalState))
    expectedForP.addTransition(Transition(finalState, S, s1))
    expectedForP.addTransition(Transition(s1, R, finalState))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForP = builder.getAutomaton(P)
    assertEquiv(actualForP, expectedForP)
  }

  test("automaton 6"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S, R), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S, Q), Q)
    )
    val expectedForQ = new NFA
    val initialState = expectedForQ.addState(true, false)
    val finalState = expectedForQ.addState(false, true)
    val s1 = expectedForQ.addState(false, false)
    val s2 = expectedForQ.addState(false, false)
    expectedForQ.addTransition(Transition(initialState, Q, finalState))
    expectedForQ.addTransition(Transition(initialState, P, s1))
    expectedForQ.addTransition(Transition(s1, S, s2))
    expectedForQ.addTransition(Transition(s1, S, initialState))
    expectedForQ.addTransition(Transition(s2, R, s1))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForQ = builder.getAutomaton(Q)
    assertEquiv(actualForQ, expectedForQ)
  }

  test("automaton 7"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, Q, R, S), P),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(Q, R), S)
    )
    val expectedForP = new NFA
    val initialState = expectedForP.addState(true, false)
    val finalState = expectedForP.addState(false, true)
    val s1 = expectedForP.addState(false, false)
    expectedForP.addTransition(Transition(initialState, P, finalState))
    expectedForP.addTransition(Transition(finalState, S, finalState))
    expectedForP.addTransition(Transition(finalState, Q, s1))
    expectedForP.addTransition(Transition(s1, R, finalState))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForP = builder.getAutomaton(P)
    assertEquiv(actualForP, expectedForP)
  }

  test("automaton 8"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectInverseOf(P), P)
    )
    val expectedForP = new NFA
    val initialStateForP = expectedForP.addState(true, false)
    val finalStateForP = expectedForP.addState(false, true)
    expectedForP.addTransition(Transition(initialStateForP, P, finalStateForP))
    expectedForP.addTransition(Transition(initialStateForP, ObjectInverseOf(P), finalStateForP))
    expectedForP.addTransition(Transition(finalStateForP, S, finalStateForP))
    expectedForP.addTransition(Transition(initialStateForP, ObjectInverseOf(S), initialStateForP))

    val builderForP = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForP = builderForP.getAutomaton(P)
    val actualForInvP = builderForP.getAutomaton(ObjectInverseOf(P))
    // Since P is symmetric, the NFAs for P and P^- should be equal.
    assertEquiv(actualForP, expectedForP)
    assertEquiv(actualForInvP, expectedForP)
  }

  test("automaton 9"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2, ObjectInverseOf(P)), Q),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(P, ObjectInverseOf(P))
    )
    val expectedForQ = new NFA
    val initialState = expectedForQ.addState(true, false)
    val finalState1 = expectedForQ.addState(false, true)
    val finalState2 = expectedForQ.addState(false, true)
    val s1 = expectedForQ.addState(false, false)
    val s2 = expectedForQ.addState(false, false)
    expectedForQ.addTransition(Transition(initialState, Q, finalState1))
    expectedForQ.addTransition(Transition(initialState, R1, s1))
    expectedForQ.addTransition(Transition(s1, R2, s2))
    expectedForQ.addTransition(Transition(s2, P, finalState2))
    expectedForQ.addTransition(Transition(s2, ObjectInverseOf(S), s2))
    expectedForQ.addTransition(Transition(s2, ObjectInverseOf(P), finalState2))
    expectedForQ.addTransition(Transition(finalState2, S, finalState2))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForQ = builder.getAutomaton(Q)
    assertEquiv(actualForQ, expectedForQ)
  }

  test("automaton 10"){
    // This test is the same as test 9, but the axiom
    //   SubObjectPropertyOfAxiom(P, ObjectInverseOf(P))
    // has been replaced with
    //   SubObjectPropertyOfAxiom(ObjectInverseOf(P), P).
    // The automaton computed should be the same.
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R2, ObjectInverseOf(P)), Q),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(P, S), P),
      SubObjectPropertyOfAxiom(ObjectInverseOf(P), P)
    )
    val expectedForQ = new NFA
    val initialState = expectedForQ.addState(true, false)
    val finalState1 = expectedForQ.addState(false, true)
    val finalState2 = expectedForQ.addState(false, true)
    val s1 = expectedForQ.addState(false, false)
    val s2 = expectedForQ.addState(false, false)
    expectedForQ.addTransition(Transition(initialState, Q, finalState1))
    expectedForQ.addTransition(Transition(initialState, R1, s1))
    expectedForQ.addTransition(Transition(s1, R2, s2))
    expectedForQ.addTransition(Transition(s2, P, finalState2))
    expectedForQ.addTransition(Transition(s2, ObjectInverseOf(S), s2))
    expectedForQ.addTransition(Transition(s2, ObjectInverseOf(P), finalState2))
    expectedForQ.addTransition(Transition(finalState2, S, finalState2))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForQ = builder.getAutomaton(Q)
    assertEquiv(actualForQ, expectedForQ)
  }

  ignore("automaton 11"){
    // Example taken from Yevgeny Kazakov, An Extension of Complex Role Inclusion Axioms in the Description Logic SROIQ. IJCAR 2010.
    val isPartOf = ObjectProperty(IRI(p, "isPartOf"))
    val isProperPartOf = ObjectProperty(IRI(p, "isProperPartOf"))
    val input = Set(
      SubObjectPropertyOfAxiom(isProperPartOf, isPartOf),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(isPartOf, isPartOf), isPartOf),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(isPartOf, isProperPartOf), isProperPartOf)
    )
    val expectedForIsPartOf = new NFA
    val initialState = expectedForIsPartOf.addState(true, false)
    val finalState1 = expectedForIsPartOf.addState(false, true)
    expectedForIsPartOf.addTransition(Transition(initialState, isPartOf, finalState1))
    expectedForIsPartOf.addTransition(Transition(initialState, isProperPartOf, finalState1))
    expectedForIsPartOf.addTransition(Transition(finalState1, isPartOf, finalState1))
    expectedForIsPartOf.addTransition(Transition(finalState1, isProperPartOf, finalState1))
    ??? // TODO: and for isProperPartOf

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForIsPartOf = builder.getAutomaton(isPartOf)
    assertEquiv(actualForIsPartOf, expectedForIsPartOf)
  }

  ignore("automaton 12") {
    // Example taken from Yevgeny Kazakov, An Extension of Complex Role Inclusion Axioms in the Description Logic SROIQ. IJCAR 2010.
    val isContainedIn = ObjectProperty(IRI(p, "isContainedIn"))
    val isNonPartitivelyContainedIn = ObjectProperty(IRI(p, "isNonPartitivelyContainedIn"))
    val input = Set(
      SubObjectPropertyOfAxiom(isNonPartitivelyContainedIn, isContainedIn),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(isContainedIn, isContainedIn), isContainedIn),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(isNonPartitivelyContainedIn, isContainedIn), isNonPartitivelyContainedIn)
    )
    val expectedForIsPartOf = new NFA
    ???

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForIsPartOf = builder.getAutomaton(isContainedIn)
    assertEquiv(actualForIsPartOf, expectedForIsPartOf)
  }

  ignore("cyclic compositions"){
    //val isPartOf = ObjectProperty(IRI(p, "isPartOf"))
    //val isProperPartOf = ObjectProperty(IRI(p, "isProperPartOf"))
    val input = Set(
      SubObjectPropertyOfAxiom(R, S1),
      SubObjectPropertyOfAxiom(S1, S2),
      SubObjectPropertyOfAxiom(S2, S3),
      SubObjectPropertyOfAxiom(S3, R),
      SubObjectPropertyOfAxiom(R, P1),
      SubObjectPropertyOfAxiom(P1, P2),
      SubObjectPropertyOfAxiom(P2, P3),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), H)
    )
    // TODO: check automata is correct, regularity tests, simplicity tests
  }

  ignore("test reflexivity"){ // TODO: include the expected output for this test (test case incomplete).
    val input = Set(
      ReflexiveObjectPropertyAxiom(R1),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R1, R1), R), // Hence R must be reflexive.
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, T), U),
      ReflexiveObjectPropertyAxiom(H),
      SubObjectPropertyOfAxiom(H, H1), // Hence H must be reflexive.
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, H1), S), // Both implicitly reflexive, thus S is reflexive.
      SubObjectPropertyOfAxiom(ObjectPropertyChain(V1, V1), V2),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(V2, V2), V1)
    )
    // TODO: test that the following are reflexive: r, h, h1, r1, r1r1, rh1, s
    // TODO: test that the following are not reflexive: t, u, rt, v1, v2, v1v1, v2v2
  }

  test("test transitivity"){
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R)
    )
    val expectedForR = new NFA
    val initialState = expectedForR.addState(true, false)
    val finalState = expectedForR.addState(false, true)
    expectedForR.addTransition(Transition(initialState, R, finalState))
    expectedForR.addTransition(Transition(finalState, R, finalState))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForR = builder.getAutomaton(R)
    assertEquiv(actualForR, expectedForR)
  }

  ignore("test transitivity with chains"){ // TODO: include the expected output for this test (test case incomplete).
    val input = Set(
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, R), R),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(S, S), S),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(T, T), T),
      SubObjectPropertyOfAxiom(ObjectPropertyChain(R, S), T)
    )
    val expectedForR = new NFA
    val initialState = expectedForR.addState(true, false)
    val finalState = expectedForR.addState(false, true)
    expectedForR.addTransition(Transition(initialState, R, finalState))
    expectedForR.addTransition(Transition(finalState, R, finalState))

    val builder = new AutomatonBuilder(convert(input), simpleSymmetryTest(input))
    val actualForR = builder.getAutomaton(T)
    assertEquiv(actualForR, expectedForR)
  }
}
