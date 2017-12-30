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
import com.sequoiareasoner.kernel.structural.DLOntology
import io.cso._
import com.sequoiareasoner.arrayops._

import scala.language.postfixOps

/** Class implementing saturation for a context.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object Context {

  private[this] def HyperRule(max: Predicate,
                              sideConditionToUse: ContextClause,
                              index: ContextClauseIndex,
                              ontology: DLOntology,
                              ordering: ContextLiteralOrdering,
                              cutting: EqualityOptimization,
                              resultsBuffer: UnprocessedDeque): Unit = {
    val iterator: Iterator[OntologyClause] = ontology.getClauses(max)
    while (iterator.hasNext) {
      val ontologyClause: OntologyClause = iterator.next
      Rules.Hyper(max, sideConditionToUse, index.centralUnifiableMaxHeadPredicateLookup, ontologyClause, ordering, cutting, ontology.isNothing, resultsBuffer)
    }
  }

  private[this] def PredRule(max: Predicate,
                             sideConditionToUse: ContextClause,
                             index: ContextClauseIndex,
                             neighborIndex: NeighborPredClauseIndex,
                             ordering: ContextLiteralOrdering,
                             resultsBuffer: UnprocessedDeque): Unit = {
    require(max.maxTerm.isFunctionTerm)
    def contextClauseLookup(p: Predicate) =
      if (p == max) IndexedSequence(sideConditionToUse)
      else index.maxHeadPredicateLookup(p)
    for (predClause <- neighborIndex(max))
      Rules.Pred(contextClauseLookup, predClause, ordering, resultsBuffer)
  }

  private[this] def EqRule(max: Predicate,
                           sideConditionToUse: ContextClause,
                           index: ContextClauseIndex,
                           ordering: ContextLiteralOrdering,
                           cutting: EqualityOptimization,
                           resultsBuffer: UnprocessedDeque): Unit = {
    for (workedOffClause <- index.maxHeadLiteralTermOccurrenceLookup(max.maxTerm))
      cforeach (workedOffClause.maxHeadLiterals) {
        case equality: Equality =>
          Rules.Eq(max, sideConditionToUse, equality, workedOffClause, ordering, cutting, resultsBuffer)
        case _ => // The Eq rule is not applicable.
      }
  }

  private[this] def EqRule(max: Equality,
                           sideConditionToUse: ContextClause,
                           index: ContextClauseIndex,
                           ordering: ContextLiteralOrdering,
                           cutting: EqualityOptimization,
                           resultsBuffer: UnprocessedDeque): Unit = {
    if (max.s == max.t) throw new IllegalStateException("The current clause is redundant!")
    for (workedOffClause <- index.maxHeadLiteralTermOccurrenceLookup(max.s))
      cforeach (workedOffClause.maxHeadLiterals) { lit =>
        if (lit.containsAtRewritePosition(max.s))
          Rules.Eq(lit, workedOffClause, max, sideConditionToUse, ordering, cutting, resultsBuffer)
      }
  }

  private[this] def EqRule(max: Inequality,
                           sideConditionToUse: ContextClause,
                           index: ContextClauseIndex,
                           ordering: ContextLiteralOrdering,
                           cutting: EqualityOptimization,
                           resultsBuffer: UnprocessedDeque): Unit = {
    if (max.s == max.t) throw new IllegalStateException("Ineq rule should have been applied immediately after clause generation!")
    for (workedOffClause <- index.maxHeadLiteralTermOccurrenceLookup(max.s))
      cforeach (workedOffClause.maxHeadLiterals) {
        case equality: Equality =>
          Rules.Eq(max, sideConditionToUse, equality, workedOffClause, ordering, cutting, resultsBuffer)
        case _ => // The Eq rule is not applicable.
      }
  }

  /** Apply rules until the current context is saturated.
    * (The Elim rule is applied immediately after each clause is derived and is not implemented as a separate rule.)
    *
    * Must be called before either `pushPredClauses` or `pushSuccClauses`.
    */
  private[this] def rulesToSaturation(state: ContextState,
                                      ontology: DLOntology,
                                      isEqualityReasoningEnabled: Boolean,
                                      ordering: ContextLiteralOrdering): Unit = {
    while (state.todo.unprocessedNonEmpty) {
      val unprocessedClause: ContextClause = state.todo.nextUnprocessed
      /* The Hyper rule will not be applicable if max == C(y) since C(z_i) occurs in neither the body of an ontology
       * clause nor the body of a context clause. */
      cforeach(unprocessedClause.maxHeadLiterals) {
        case p: Predicate =>
          // Since Predicate is the most common case, it is at the top of the match for efficiency.
          if (p.hasCentralVariable) {
            HyperRule(p, unprocessedClause, state.workedOffClauses, ontology, ordering, state.cutting, state.resultsBuffer)
            while (state.resultsBuffer.nonEmpty)
              state.hyperConclusion(state.resultsBuffer.removeFirst)
          }
          if (p.maxTerm.isFunctionTerm) {
            PredRule(p, unprocessedClause, state.workedOffClauses, state.neighborIndex, ordering, state.resultsBuffer)
            while (state.resultsBuffer.nonEmpty)
              state.predConclusion(state.resultsBuffer.removeFirst)
            if (isEqualityReasoningEnabled) {
              EqRule(p, unprocessedClause, state.workedOffClauses, ordering, state.cutting, state.resultsBuffer)
              while (state.resultsBuffer.nonEmpty)
                state.eqConclusion(state.resultsBuffer.removeFirst)
            }
          }
        case lit: Equality =>
          if (isEqualityReasoningEnabled) {
            EqRule(lit, unprocessedClause, state.workedOffClauses, ordering, state.cutting, state.resultsBuffer)
            while (state.resultsBuffer.nonEmpty)
              state.eqConclusion(state.resultsBuffer.removeFirst)
          }
        case lit: Inequality =>
          if (isEqualityReasoningEnabled) {
            EqRule(lit, unprocessedClause, state.workedOffClauses, ordering, state.cutting, state.resultsBuffer)
            while (state.resultsBuffer.nonEmpty)
              state.eqConclusion(state.resultsBuffer.removeFirst)
          }
      }
      state.workedOffClauses.add(unprocessedClause)
    }
  }

  private[this] def pushPredClausesDerivedInLastRound(state: ContextState,
                                                      contextStructureManager: ContextStructureManager): Unit = {
    while (state.predClausesOnLastRound.nonEmpty) {
      val clause: ContextClause = state.predClausesOnLastRound.removeFirst
      state.workedOffClauses.updatePredIndex(clause, add = true)
      for ((incomingEdge: UnboundedChannel[InterContextMessage], edgeLabel: Term) <- state.getContextStructurePredecessors(clause.body)) {
        contextStructureManager.contextRoundStarted
        incomingEdge ! PredPush(edgeLabel, state.core, IndexedSequence(clause))
      }
    }
  }

  private[this] def pushWorkedOffPredClauses(state: ContextState,
                                             contextStructureManager: ContextStructureManager,
                                             contextChannel: UnboundedChannel[InterContextMessage],
                                             edgeLabel: Term,
                                             predicate: Predicate): Unit = {
    // Changes to worked off clauses will not be visible in predClauses; hence it can be communicated to the neighbour.
    val predClauses: IndexedSequence[ContextClause] =
      if (state.core contains predicate) state.workedOffClauses.allPredClauses
      else state.workedOffClauses.bodyPredicateLookupPredClauses(predicate)
    if (predClauses.nonEmpty) {
      contextStructureManager.contextRoundStarted
      contextChannel ! PredPush(edgeLabel, state.core, predClauses)
    }
  }

  private[this] def pushSuccClausesDerivedInLastRound(state: ContextState,
                                                      ontology: DLOntology,
                                                      contextStructureManager: ContextStructureManager,
                                                      contextChannel: UnboundedChannel[InterContextMessage]): Unit = {
    /* Delaying pushing of Succ clauses helps with design patterns used in modelling of 00003.
     * Also increases the determinism of the procedure in case Hyper pulls in a clause with a disjunct that
     * eliminates the need for pushing predicates forward.
     */
    state.forEachNewSuccPredicate { p =>
      val t = p.maxTerm
      assert(t.isFunctionTerm)
      val sigma = new ForwardsInterContextMapping(t)
      val pSigma = p.applySubstitution(sigma)
      // Neighbour will have to do another round.
      contextStructureManager.contextRoundStarted
      /* Implementing the Succ rule of the calculus in the generality allowed for in the paper might be too
       * expensive in practice: If the neighbour already exists then one would have to query the neighbour process for
       * whether T -> T are contained in the neighbour for each T in K2 (one communication), and then read the reply
       * (another communication). If the reply is NO, then this process would then query the strategy (one
       * communication), and then read the reply (another communication). In practice, it is likely that the strategy
       * will return the same neighbour as we started with, and it is also quite unlikely that the core of a new
       * neighbour would have been a strict superset of the exiting neighbour (i.e. it is unlikely that any of the
       * predicates in K2 resulted from Horn clauses). It is much cheaper (in terms of inter-process communication) to
       * send one message to the existing neighbour and to not have to wait for any reply.
       *
       * Furthermore, if you get a context with a new core for the term `t` you have to reinitialize the context with
       * the predicates for overloading.
       */
      val edge: UnboundedChannel[InterContextMessage] =
        state.getSuccessorOrElseUpdate(t, {
          // Create new successor by communicating with Strategy.
          val K1: ImmutableSet[Predicate] = ontology.getKnownPredicates(t)
          contextStructureManager.getSuccessor(K1)
        })
      edge ! SuccPush(contextChannel, t, pSigma)
    }
  }

  // TODO: Optimisation: If derive Horn clause containing t1 == t2 in head and t1 > t2, then rewrite every t1 to t2 immediately.
  def context(state: ContextState,
              ontology: DLOntology,
              isEqualityReasoningEnabled: Boolean,
              order: ContextLiteralOrdering,
              contextStructureManager: ContextStructureManager,
              incoming: UnboundedChannel[InterContextMessage]): Proc = proc (state.core.mkString("[",", ","]")) {

    // Core rule
    if (state.core.exists( p => ontology.isNothing(p) ))
      state.coreConclusion(ContextClause(ArrayBuilders.emptyPredicateArray, ArrayBuilders.emptyLiteralArray)(order))
    else
      for (p <- state.core) state.coreConclusion(ContextClause(ArrayBuilders.emptyPredicateArray, Array(p))(order))

    // Perform Hyper rule on clauses from the ontology with an empty body.
    // Since the body is empty, the head cannot contain any neighbour variables.
    for (OntologyClause(_, head) <- ontology.getFacts)
      state.hyperConclusion(ContextClause(ArrayBuilders.emptyPredicateArray, head)(order))

    rulesToSaturation(state, ontology, isEqualityReasoningEnabled, order)
    pushPredClausesDerivedInLastRound(state, contextStructureManager)
    pushSuccClausesDerivedInLastRound(state, ontology, contextStructureManager, incoming)
    contextStructureManager.contextRoundFinished
    repeat {
      incoming ? match {
        case SuccPush(contextChannel: UnboundedChannel[InterContextMessage],
                      edgeLabel: Term,
                      predicate: Predicate) =>
          // Add predecessor (which will also unblock pred clauses before the Succ rule is applied).
          val hasUnblocked = state.addContextStructurePredecessor(contextChannel, edgeLabel, predicate)
          // Succ rule
          val clauseAdded = state.succConclusion(ContextClause(Array(predicate), Array(predicate))(order))
          if (!clauseAdded) {
            // Push the worked off pred clauses first before pushPredClausesDerivedInLastRound to avoid repetition.
            pushWorkedOffPredClauses(state, contextStructureManager, contextChannel, edgeLabel, predicate)
          }
          if (clauseAdded || hasUnblocked) {
            // Saturate if any clauses added.
            rulesToSaturation(state, ontology, isEqualityReasoningEnabled, order)
            pushPredClausesDerivedInLastRound(state, contextStructureManager)
            pushSuccClausesDerivedInLastRound(state, ontology, contextStructureManager, incoming)
          }
        case PredPush(edgeLabel: Term,
                      neighbourCore: ImmutableSet[Predicate],
                      predClauses: IndexedSequence[ContextClause]) =>
          predClauses foreach { clause =>
            val predClause: PredClause = state.neighborIndex.add(edgeLabel, neighbourCore, clause)
            Rules.Pred(state.workedOffClauses.maxHeadPredicateLookup, predClause, order, state.resultsBuffer)
            while (state.resultsBuffer.nonEmpty)
              state.predConclusion(state.resultsBuffer.removeFirst)
          }
          rulesToSaturation(state, ontology, isEqualityReasoningEnabled, order)
          pushPredClausesDerivedInLastRound(state, contextStructureManager)
          pushSuccClausesDerivedInLastRound(state, ontology, contextStructureManager, incoming)
      }
      contextStructureManager.contextRoundFinished
    }

  }

}
