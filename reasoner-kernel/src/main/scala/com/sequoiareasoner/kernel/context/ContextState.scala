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

package com.sequoiareasoner.kernel.context

import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.index._
import io.cso._
import com.sequoiareasoner.arrayops._

import scala.collection.mutable

/** Class that combines all data structures used to maintain the state of the derivation within a context.
  * In particular, this class holds both the unprocessed queue of clauses and the set of worked off clauses for a
  * single context.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param core
  * @param workedOffClauses
  * @param neighborIndex
  * @param cutting
  * @param redundancyIndex
  */
class ContextState(val core: ImmutableSet[Predicate],
                   val workedOffClauses: ContextClauseIndex,
                   val neighborIndex: NeighborPredClauseIndex,
                   val cutting: EqualityOptimization,
                   redundancyIndex: ContextClauseRedundancyIndex) extends RuleConclusionObserver {

  // Check that the definition of core from the technical report is adhered to.
  require { core forall { l => l.isLegalInContextClauseBody } }

  /** The set of unprocessed clauses not yet in worked-off. */
  val todo = new UnprocessedClauses

  /** The set of clauses derived on the last round. */
  val predClausesOnLastRound = new UnprocessedDeque

  /** The buffer into which rule conclusion results are collected. Reusing a single buffer avoids object churn. */
  val resultsBuffer = new UnprocessedDeque

  private[this] class Counter {
    private[this] var count = 0
    def nonZero: Boolean = count > 0
    def increment: Unit = count += 1
    def decrement: Unit = {
      count -= 1
      if (count < 0) throw new AssertionError
    }
  }
  private[this] val succPredicates = new mutable.AnyRefMap[Predicate, Counter]
  private[this] val pushedSucc = new mutable.HashSet[Predicate]
  private[this] def incSuccPredicateCount(predicate: Predicate): Unit =
    succPredicates.getOrElseUpdate(predicate, new Counter).increment
  private[this] def decSuccPredicateCount(predicate: Predicate): Unit =
    succPredicates(predicate).decrement

  // TODO: if a predicate is no longer relevant for succ, then retract from successor to optimize pred rule.
  def forEachNewSuccPredicate(f: Predicate => Unit): Unit =
    for ((p, counter) <- succPredicates) if (counter.nonZero && pushedSucc.add(p)) f(p)


  // A map from each term f(x) to the corresponding edge (i.e., the message passing channel for the successor f(x))
  private[this] val successors = new mutable.LongMap[UnboundedChannel[InterContextMessage]]

  def getSuccessorOrElseUpdate(t: Term, default: => UnboundedChannel[InterContextMessage]): UnboundedChannel[InterContextMessage] =
    successors.getOrElseUpdate(t.id, default)

  private[this] val predecessors = new mutable.AnyRefMap[(UnboundedChannel[InterContextMessage], Term), mutable.Set[Predicate]]

  /**
    *
    * @param incomingEdge
    * @param edgeLabel
    * @param p
    * @return `true` if some clause is unblocked
    */
  def addContextStructurePredecessor(incomingEdge: UnboundedChannel[InterContextMessage], edgeLabel: Term, p: Predicate): Boolean = {
    require(edgeLabel.isFunctionTerm)
    val predicates = predecessors.getOrElseUpdate((incomingEdge, edgeLabel), new mutable.HashSet[Predicate])
    predicates += p
    // Remove from blockedClauseIndex and enqueue.
    var hasUnblocked = false
    blockedClauseIndex.retrieveAndRemoveClauses(predicates.toArray, {
      clause =>
        hasUnblocked = addIfNotInStrengthening(clause) || hasUnblocked
        updatePredSuccTodo(clause)
    })
    hasUnblocked
  }

  def getContextStructurePredecessors(body: Array[Predicate]): Iterable[((UnboundedChannel[InterContextMessage], Term))] =
    predecessors collect {
      case (pair, atoms) if /*body.toSet.subsetOf(atoms)*/ cforall (body) {atoms contains _} => pair
    }

  private[this] val blockedClauseIndex = new BlockedContextClauseIndex

  /**
    *
    * @param clause
    * @return `true` if the clause was blocked, `false` otherwise
    */
  private[this] def blockIfNotUseful(clause: ContextClause): Boolean = {
    if (clause.body.length > 1) {
      val bodySet = clause.body.toSet
      val isUseful = predecessors.values.exists( set => bodySet.subsetOf(set) )
      if (!isUseful) {
        blockedClauseIndex.add(clause)
        return true
      }
    }
    return false
  }

  private[this] def addIfNotInStrengthening(clause: ContextClause): Boolean = {
    if (redundancyIndex.isClauseSubsumed(clause)) return false
    // At this point we know the clause needs to be added, and that it may subsume a previously derived clause.
    redundancyIndex.removeSubsumedClauses(clause) { subsumedClause =>
      // redundancyIndex contains clauses in both the unprocessed queue and the worked off clauses.
      workedOffClauses.remove(subsumedClause)
      // Update Pred and Succ indexes.
      if (subsumedClause.isClauseHeadForPred) workedOffClauses.updatePredIndex(subsumedClause, add = false)
      cforeach (subsumedClause.maxHeadPredicates) { p => if (p.isSuccTrigger) decSuccPredicateCount(p) }
    }
    redundancyIndex.add(clause)
    cutting.clauseDerived(clause)
    todo.addUnprocessedClause(clause)
    true
  }

  private[this] def updatePredSuccTodo(clause: ContextClause): Unit = {
    /* predClausesOnLastRound will be empty at the start of the round, so at the end of the round,
     * predClausesOnLastRound will only contain non-redundant clauses. */
    if (clause.isClauseHeadForPred) {
      predClausesOnLastRound.removeRedundant(clause)
      predClausesOnLastRound.addLast(clause)
    }
    cforeach (clause.maxHeadPredicates) { p => if (p.isSuccTrigger) incSuccPredicateCount(p) }
  }

  override def coreConclusion(clause: ContextClause): Boolean = {
    require(clause.isFact)
    /* Optimization: Skip redundancy checks. */
    redundancyIndex.add(clause)
    todo.addUnprocessedClause(clause)
    true
  }

  override def succConclusion(clause: ContextClause): Boolean = {
    /* Optimization: A clause which is a conclusion of the Succ rule need not participate in the Pred rule for the
     * predecessor (it would be redundant). Also, the Succ rule does not derive tautologies in the head. */
    addIfNotInStrengthening(clause)
  }

  override def hyperConclusion(clause: ContextClause): Boolean =
    !clause.isHeadTautology && {
      val added = !blockIfNotUseful(clause) && addIfNotInStrengthening(clause)
      if (added) {
        // The only tautological clauses allowed by redundancy elimination are those introduced by Core and Succ.
        assert(cforall(clause.head) { l: Literal => !ccontains(clause.body, l) })
        updatePredSuccTodo(clause)
      }
      added
    }

  override def predConclusion(clause: ContextClause): Boolean =
    !clause.isHeadTautology && {
      /* Optimization: A clause which is a conclusion of the Pred rule cannot participate in the Pred rule for the predecessor. */
      val added = !blockIfNotUseful(clause) && addIfNotInStrengthening(clause)
      if (added) {
        // The only tautological clauses allowed by redundancy elimination are those introduced by Core and Succ.
        assert(cforall(clause.head) { l: Literal => !ccontains(clause.body, l) })
        updatePredSuccTodo(clause)
      }
      added
    }

  override def eqConclusion(clause: ContextClause): Boolean =
    !clause.isHeadTautology && {
      val added = !blockIfNotUseful(clause) && addIfNotInStrengthening(clause)
      if (added) {
        // The only tautological clauses allowed by redundancy elimination are those introduced by Core and Succ.
        assert(cforall(clause.head) { l: Literal => !ccontains(clause.body, l) })
        updatePredSuccTodo(clause)
      }
      added
    }

}
