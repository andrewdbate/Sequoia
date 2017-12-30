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

import scala.collection.mutable
import scala.language.implicitConversions

object AutomatonBuilder {

  final case class RoleInclusionAxiom(subObjectProperties: Seq[ObjectPropertyExpression], superObjectProperty: ObjectPropertyExpression)

  /** The type of non-deterministic finite automaton */
  type NFA = rationals.Automaton[ObjectPropertyExpression, rationals.Transition[ObjectPropertyExpression], Builder]
  type Reducer = rationals.transformations.Reducer[ObjectPropertyExpression, rationals.Transition[ObjectPropertyExpression], Builder]
  type EpsilonTransitionRemover = rationals.transformations.EpsilonTransitionRemover[ObjectPropertyExpression, rationals.Transition[ObjectPropertyExpression], Builder]
  type Normalizer = rationals.transformations.Normalizer[ObjectPropertyExpression, rationals.Transition[ObjectPropertyExpression], Builder]
  type State = rationals.State
  type Builder = rationals.TransitionBuilder[ObjectPropertyExpression]

  case class Transition(start: State, label: ObjectPropertyExpression, end: State)

  implicit def fromRational(t: rationals.Transition[ObjectPropertyExpression]): Transition =
    Transition(t.start, t.label, t.end)

  implicit def fromRationalCollection1(ts: java.util.Collection[rationals.Transition[ObjectPropertyExpression]]): Set[Transition] = {
    val it = ts.iterator
    val result = new mutable.HashSet[Transition]
    while (it.hasNext) {
      val next = it.next
      result += fromRational(next)
    }
    result.toSet
  }

  implicit def fromRationalCollection2(ts: java.util.Collection[State]): Set[State] = {
    val it = ts.iterator
    val result = new mutable.HashSet[State]
    while (it.hasNext) result += it.next
    result.toSet
  }

  implicit def toRational(t: Transition) =
    new rationals.Transition(t.start, t.label, t.end)

  /** Given an NFA `automaton` with edges labelled by object property expressions, returns the NFA `automaton'` define
    * as follows:
    *  $ - the initial states of `automaton'` are the terminate states of `automaton`
    *  $ - the terminates states of `automaton'` are the initial states of `automaton`
    *  $ - for each epsilon transition (u, v) in `automaton`, the NFA `automaton'` contains the epsilon transition (v, u).
    *  $ - for each labelled transition (u, ope, v) in `automaton`, the NFA `automaton'` contains the transition (v, ope', u)
    *      where `ope'` is the inverse of the object property expression `ope`.
    *
    * We call this construction the mirror image of `automaton`.
    *
    * @param automaton  the input automation
    * @return the mirror image of the specified automation
    */
  def getMirroredCopy(automaton: NFA): NFA = {
    val mirroredCopy = new NFA
    val map = new mutable.HashMap[State, State]
    for (state: State <- automaton.states)
      map.put(state, mirroredCopy.addState(state.isTerminal, state.isInitial))
    for (Transition(start, label, end) <- automaton.delta) label match {
      case lbl: ObjectPropertyExpression =>
        mirroredCopy.addTransition(Transition(map(end), lbl.inverse, map(start)))
      case epsilon                       =>
        assert(epsilon == null)
        mirroredCopy.addTransition(Transition(map(end), null, map(start)))
    }
    mirroredCopy
  }

  def defaultDFA(op: ObjectProperty): NFA = {
    val automaton = new NFA
    val initialState = automaton.addState(true, false)
    val finalState = automaton.addState(false, true)
    automaton.addTransition(Transition(initialState, op, finalState))
    automaton
  }

  private def reduce(nfa: NFA): NFA = {
    val f = new Reducer
    f.transform(nfa)
  }

  private def removeEpsilonTransitions(nfa: NFA): NFA = {
    val f = new EpsilonTransitionRemover
    f.transform(nfa)
  }

  private def normalize(nfa: NFA): NFA = {
    val f = new Normalizer
    f.transform(nfa)
  }

}

/** Given a collection of object property inclusions and symmetric properties, this class can be used to construct the
  * non-deterministic finite automation corresponding to a given object property expression.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param opeInclusions
  * @param isSymmetric
  */
final class AutomatonBuilder(opeInclusions: Iterable[AutomatonBuilder.RoleInclusionAxiom], isSymmetric: ObjectProperty => Boolean) {
  import AutomatonBuilder._

  /** Chains a mapping R -> w for each role inclusion axiom w \sqsubseteq R in the RBox where w is of one of the forms
    * $ - w = RR
    * $ - w = R_1 ... R_n and R_1 \neq R \neq R_n
    * $ - w = R R_1 ... R_n
    * $ - w = R_1 ... R_n-1 R
    */
  private[this] val roleToRia: mutable.MultiMap[ObjectProperty, Seq[ObjectPropertyExpression]] =
    new mutable.HashMap[ObjectProperty, mutable.Set[Seq[ObjectPropertyExpression]]] with mutable.MultiMap[ObjectProperty, Seq[ObjectPropertyExpression]] {
      override def default(key: ObjectProperty) = mutable.Set.empty
    }

  // Filter out the symmetry axioms collected by symmetricRoles (needed to eliminate infinite recursion during NFA construction).
  for (RoleInclusionAxiom(chain, rhs) <- opeInclusions if chain != Seq(rhs.inverse))
    rhs match {
      case op: ObjectProperty => roleToRia.addBinding(op, chain)
      case ObjectInverseOf(op) => roleToRia.addBinding(op, chain.map{_.inverse}.reverse)
    }

  private[this] val roleToAutomaton = new mutable.AnyRefMap[ObjectPropertyExpression, NFA]

  def getAutomaton(r: ObjectPropertyExpression): NFA = roleToAutomaton.getOrElseUpdate(r, r match {
    case r: ObjectProperty =>

      /* Build the automaton, avoiding the introduction of epsilon edges where possible. Avoiding epsilon transitions
       * is necessary, since the redundant clauses that will result from it impact the performance of the calculus.
       *
       * The alternative is to construct the automaton using epsilon transition (as described in The Even More
       * Irresistible SROIQ, Ian Horrocks, Oliver Kutz, and Ulrike Sattler, Technical Report, 2005), however, the
       * elimination of epsilon edges using the standard algorithm is very slow for large automata.
       */
      def build(nfa: NFA, seed: Set[State], chain: Seq[ObjectPropertyExpression]): Set[State] =
        chain.foldLeft(seed) {
          case (lastFinals, ope) =>
            val currentNFA = getAutomaton(ope)
            val currentInitials = currentNFA.initials
            val currentFinals = currentNFA.terminals
            val m = new mutable.AnyRefMap[State, State]
            for (s: State <- currentNFA.states)
              m.put(s, nfa.addState(false, false))
            for (Transition(start, label, end) <- currentNFA.delta)
              nfa.addTransition(Transition(m(start), label, m(end)))
            // Add the epsilon transitions
            for (fin <- lastFinals; init <- currentInitials)
              nfa.addTransition(Transition(fin, null, m(init)))
            currentFinals map m
        }

      def buildForRole: NFA = {
        val nfa = defaultDFA(r)
        val initialStates = nfa.initials
        val finalStates = nfa.terminals
        for (chain <- roleToRia(r)) chain match {
          case Seq(`r`, `r`) =>
            // RR -> R or R^- R^- -> R^-
            for (fin <- finalStates; init <- initialStates)
              nfa.addTransition(Transition(fin, null, init))
          case `r` +: rest =>
              // R S2...Sn -> R
            for (lastState <- build(nfa, finalStates, rest); fin <- finalStates)
              nfa.addTransition(Transition(lastState, null, fin))
          case init :+ `r` =>
            // S1...Sn-1 R -> R
            for (lastState <- build(nfa, initialStates, init); init <- initialStates)
              nfa.addTransition(Transition(lastState, null, init))
          case chain =>
            // S1...Sn -> R
            for (lastState <- build(nfa, initialStates, chain); fin <- finalStates)
              nfa.addTransition(Transition(lastState, null, fin))
        }
        nfa
      }

      val finalNFA = if (isSymmetric(r)) {
        val nfa = buildForRole
        val nfaInitials = nfa.initials
        val nfaFinals = nfa.terminals
        val mirroredNFA = getMirroredCopy(nfa)
        /* Ian's paper, cited above, would handle the case of R^- -> R, by adding the following edges:
         *  - an epsilon edge from each initial state of `nfa` to each initial state of `mirroredNFA`;
         *  - an epsilon edge from each initial state of `mirroredNFA` to each initial state of `nfa`;
         *  - an epsilon edge from each terminal state of `nfa` to each terminal state of `mirroredNFA`;
         *  - an epsilon edge from each terminal state of `mirroredNFA` to each terminal state of `nfa`;
         * It is more efficient to eliminate the epsilon edges immediately by immediately reusing the initial states
         * and terminal states of `nfa` as implemented below. This reduces the amount of work required to eliminate
         * epsilon edges from the final NFA.
         */
        val map = new mutable.AnyRefMap[State, Set[State]]
        for (s: State <- mirroredNFA.states)
          (s.isInitial, s.isTerminal) match {
            case (true, true)   => map.put(s, nfaInitials ++ nfaFinals)
            case (true, false)  => map.put(s, nfaInitials)
            case (false, true)  => map.put(s, nfaFinals)
            case (false, false) => map.put(s, Set(nfa.addState(false, false)))
          }
        for (Transition(start, label, end) <- mirroredNFA.delta;
             source <- map(start);
             sink <- map(end))
          nfa.addTransition(Transition(source, label, sink))
        nfa
      } else {
        buildForRole
      }

      reduce(finalNFA)

    case ObjectInverseOf(r) => getMirroredCopy(getAutomaton(r))
  })

}
