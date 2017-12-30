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

package com.sequoiareasoner.kernel.context

import com.sequoiareasoner.kernel.index._
import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.arrayops._

/** This class maintains the data structures that are used to index the clauses derived within a context, that is, this
  * class provides an indexed for the worked off set of clauses for a single context.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class ContextClauseIndex {
  import Term.{x, y}

  /** Map from a central unifier pattern of a predicate to the clauses in which that predicate occurs maximally in the head up to a central substitution. */
  private[this] val centralUnifiableMaxHeadPredicateIndex = new LongIndexedSequenceMap[ContextClause](8, ArrayBuilders.contextClauseArrayBuilder)

  /** Map from a predicate uid to the clauses in which that predicate occurs maximally in the head. */
  private[this] val maxHeadPredicateIndex = new LongIndexedSequenceMap[ContextClause](8, ArrayBuilders.contextClauseArrayBuilder)

  /** Maps each function symbol f(x) to the set of clauses that mention f(x) in some maximal literal in the head. */
  private[this] val maxHeadLiteralTermIndex = new LongIndexedSequenceMap[ContextClause](8, ArrayBuilders.contextClauseArrayBuilder)

  /** Maps each function symbol f(x) to the set of predicate that appear in facts that mention f(x). */
  private[this] val k1Index = new LongIndexedSequenceMap[Predicate](8, ArrayBuilders.predicateArrayBuilder)

  /** Returns a (possibly empty) indexed sequence of context clauses, each of which contains a predicate in the head
    * that can unify with `p` using a central substitution.
    *
    * @param p  a predicate occurring in the body of some ontology clause.
    * @return all context clauses that contain a *maximal* predicate that can unify with `p` using a central substitution.
    */
  def centralUnifiableMaxHeadPredicateLookup(p: Predicate): IndexedSequence[ContextClause] = {
    // `p` comes from an ontology clause when doing lookup in the Hyper rule.
    require(p.isLegalInOntologyClauseBody, s"$p is not legal in an ontology clause body.")
    centralUnifiableMaxHeadPredicateIndex(p.centralUnifierPattern)
  }

  /** Returns a (possibly empty) indexed sequence of context clauses, each of which contains the predicate `p` in the head.
    *
    * @param p  a predicate that is legal to occur in the head of a context clause.
    * @return all context clauses that contain `p` maximally in the head.
    */
  def maxHeadPredicateLookup(p: Predicate): IndexedSequence[ContextClause] = {
    // `p` comes from an ontology clause when doing lookup in the Pred rule.
    require(p.isLegalInContextClauseHead, s"$p is not legal in a context clause body.")
    maxHeadPredicateIndex(p.uid)
  }

  /** Returns a (possibly empty) indexed sequence of context clauses, each of which contains a literal in the the head
    * that contains the term `t`. Throws an [[IllegalArgumentException]] if `t` is not of the form `f(x)`.
    *
    * @param t a functional term
    * @return the set of context clauses that mention `t` in some literal in the head.
    */
  def maxHeadLiteralTermOccurrenceLookup(t: Term): IndexedSequence[ContextClause] = {
    require(t.isFunctionTerm)
    maxHeadLiteralTermIndex(t.id)
  }

  def K1(t: Term): ImmutableSet[Predicate] = {
    require(t.isFunctionTerm)
    ImmutableSet(k1Index(t.id))
  }

  private[this] def addToK1Index(c: ContextClause): Unit = {
    val ContextClause(body, head) = c
    if (body.length == 0 && head.length == 1) head(0) match {
      case c @ Concept(_, t) =>
        val pred = c.copy(t = x)
        k1Index.addBinding(t.id, pred)
      case r @ Role(_, s, t) if s.isCentralVariable =>
        val pred = r.copy(s = y, t = x)
        k1Index.addBinding(t.id, pred)
      case r @ Role(_, s, t) if t.isCentralVariable =>
        val pred = r.copy(s = x, t = y)
        k1Index.addBinding(s.id, pred)
      case e: Equation => // Do nothing
    }
  }

  /**
   *
   * @param c
   * @param add `true` to add `c` to the indexes; `false` to remove `c` from the indexes
   */
  private[this] def updateIndex(c: ContextClause, add: Boolean): Unit = {
    @inline def update(key: Long, index: LongIndexedSequenceMap[ContextClause]): Unit =
      if (add) index.addBinding(key, c)
      else index.removeBinding(key, c)

    if (add) addToK1Index(c)

    if (c.head.length > 0) { // TODO: c.head.length == 0
      if (add) {
        cforeach (c.maxHeadPredicates) { p => maxHeadPredicateIndex.addBinding(p.uid, c) }
      } else {
        cforeach (c.maxHeadPredicates) { p => maxHeadPredicateIndex.removeBinding(p.uid, c) }
      }

      // Only the max head literals needs to be added to the index.
      cforeach (c.maxHeadLiterals) {
        case p @ Concept(_, t) =>
          assert(p.isLegalInContextClauseHead)
          update(p.centralUnifierPattern, centralUnifiableMaxHeadPredicateIndex)
          if (t.isFunctionTerm) update(t.id, maxHeadLiteralTermIndex)
        case p @ Role(_, s, t) if s.isCentralVariable && t.isCentralVariable =>
          assert(p.isLegalInContextClauseHead)
          update(p.centralUnifierPattern, centralUnifiableMaxHeadPredicateIndex)
          // [BEGIN] HACK
          val R1 = p.rewrite(s, Term.z(1))
          update(R1.centralUnifierPattern, centralUnifiableMaxHeadPredicateIndex)
          val R2 = p.rewrite(t, Term.z(1))
          update(R2.centralUnifierPattern, centralUnifiableMaxHeadPredicateIndex)
          // [END] HACK
        case p @ Role(_, s, t) if s.isCentralVariable =>
          assert(p.isLegalInContextClauseHead)
          update(p.centralUnifierPattern, centralUnifiableMaxHeadPredicateIndex)
          if (t.isFunctionTerm) update(t.id, maxHeadLiteralTermIndex)
        case p @ Role(_, s, t) if t.isCentralVariable =>
          assert(p.isLegalInContextClauseHead)
          update(p.centralUnifierPattern, centralUnifiableMaxHeadPredicateIndex)
          if (s.isFunctionTerm) update(s.id, maxHeadLiteralTermIndex)
        case e: Equation =>
          val maxTerm = e.s
          assert(maxTerm.isFunctionTerm, s"$e is illegal!")
          update(maxTerm.id, maxHeadLiteralTermIndex)
      }
    }
  }

  /** Add a context clause to this index. It is assumed that the clause is *not* redundant because redundancy
    * checks are implemented externally.
    *
    * @param c
    */
  def add(c: ContextClause): Unit = updateIndex(c, true)

  /** Remove a context clause from this index.
    *
    * @param c
    */
  def remove(c: ContextClause): Unit = updateIndex(c, false)

  /** Map from a predicate `p` to the clauses that contain `p` in the body, and each atom in the head is a pred trigger. */
  private[this] val predIndex = new LongIndexedSequenceMap[ContextClause](8, ArrayBuilders.contextClauseArrayBuilder)

  /** The clauses derived in this context such that each atom in the head is a pred trigger. */
  private[this] val predClauses = new ArrayIndexedSequence[ContextClause](8, ArrayBuilders.contextClauseArrayBuilder)

  /** Returns a (possibly empty) indexed sequence of context clauses, each of which contains the predicate `p` in the
    * body and each literal in the head is a trigger for the Pred rule in a predecessor context.
    *
    * Subsequent changes to this index will not be visible in the collection returned.
    *
    * @param p  a predicate occurring in the body of some context clause.
    * @return all context clauses that are pred triggers and that contain `p` in the body.
    */
  def bodyPredicateLookupPredClauses(p: Predicate): IndexedSequence[ContextClause] = { // TODO: consider returning an immutable collection.
    require(p.isLegalInContextClauseBody, s"$p is not legal in a context clause body.")
    predIndex.copyOf(p.uid)
  }

  /** Returns a (possibly empty) indexed sequence of context clauses, each of which is such that each literal in the
    * head is a trigger for the Pred rule in a predecessor context.
    *
    * Subsequent changes to this index will not be visible in the collection returned.
    *
    * @return all context clauses that are pred triggers.
    */
  def allPredClauses: IndexedSequence[ContextClause] = predClauses.copy // TODO: consider returning an immutable collection.

  /**
    *
    * @param clause  the clause that pay participate in an application of the Pred rule with a predecessor.
    * @param add `true` to add `c` to the indexes; `false` to remove `c` from the indexes
    */
  def updatePredIndex(clause: ContextClause, add: Boolean): Unit = {
    // TODO: when adding a clause, also remove the redundant clauses.
    @inline def update(key: Long, index: LongIndexedSequenceMap[ContextClause]): Unit =
      if (add) index.addBinding(key, clause)
      else index.removeBinding(key, clause)

    if (add) {
      predClauses += clause
      cforeach (clause.body) { p => predIndex.addBinding(p.uid, clause) }
    } else {
      predClauses -= clause
      cforeach (clause.body) { p => predIndex.removeBinding(p.uid, clause) }
    }
  }

}
