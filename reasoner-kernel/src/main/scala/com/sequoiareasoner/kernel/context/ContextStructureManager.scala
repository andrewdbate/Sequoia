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

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.index.{ArrayBuilders, ImmutableSet, NeighborPredClauseIndex, TotalIRIMultiMap, _}
import com.sequoiareasoner.kernel.logging.Logger
import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.structural.DLOntology
import com.sequoiareasoner.kernel.taxonomy.Taxonomy
import io.cso._

import scala.collection.mutable

object ContextStructureManager {

  def trivialStrategy(predicates: ImmutableSet[Predicate]): ImmutableSet[Predicate] = ImmutableSet.empty

  def centralStrategy(predicates: ImmutableSet[Predicate]): ImmutableSet[Predicate] = predicates filter {
    case Concept(_, _) => true
    case _ => false
  }

  def safeCentralStrategy(predicates: ImmutableSet[Predicate]): ImmutableSet[Predicate] = {
    val core = centralStrategy(predicates)
    if (core.isEmpty) predicates else core
  }

  def eagerStrategy(predicates: ImmutableSet[Predicate]): ImmutableSet[Predicate] = predicates

}

/** Class that manages the context structure, including the introducing of new contexts according to a supplied strategy
  * function, and the termination of the procedure once all contexts have completed saturation.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param ontology
  * @param redundancyIndex
  * @param enableEqualityReasoning
  * @param equalityOptimization
  * @param strategy
  */
final class ContextStructureManager(ontology: DLOntology,
                                    redundancyIndex: => ContextClauseRedundancyIndex,
                                    enableEqualityReasoning: Boolean,
                                    equalityOptimization: => EqualityOptimization,
                                    strategy: ImmutableSet[Predicate] => ImmutableSet[Predicate],
                                    logger: Logger) {
  if (ontology eq null) throw new NullPointerException

  private[this] var beginTime: Long = 0L
  private[this] var totalTime: Long = 0L

  private[this] val contexts = new mutable.AnyRefMap[ImmutableSet[Predicate], UnboundedChannel[InterContextMessage]]
  private[this] val conceptsToClassify: Set[Concept] = ontology.getConceptsToClassify
  private[this] val activeCount = new AtomicInteger(conceptsToClassify.size)
  private[this] val latch = new CountDownLatch(1)

  /** `superConcepts(a)` returns all the direct and non-direct super concepts of `a`. */
  private[this] val superConcepts = new TotalIRIMultiMap[IRI](ArrayBuilders.iriArrayBuilder)

  private[this] def buildContext(core: ImmutableSet[Predicate], workedOffClauseIndex: ContextClauseIndex, edge: UnboundedChannel[InterContextMessage], ordering: ContextLiteralOrdering): Proc = {
    val state = new ContextState(core, workedOffClauseIndex, new NeighborPredClauseIndex, equalityOptimization, redundancyIndex) //with RuleConclusionLogger
    Context.context(state, ontology, enableEqualityReasoning, ordering, this, edge)
  }

  protected[context] def contextRoundStarted: Unit = activeCount.incrementAndGet

  protected[context] def contextRoundFinished: Unit = {
    val count = activeCount.decrementAndGet
    if (count < 0) throw new IllegalStateException
    if (count == 0) {
      totalTime = System.currentTimeMillis - beginTime
      logger.info(s"Saturation completed in $totalTime.")
      latch.countDown
    }
  }

  synchronized {
    beginTime = System.currentTimeMillis
    // Initialise by forking the root contexts.
    for (concept <- conceptsToClassify) {
      val core: ImmutableSet[Predicate] = ImmutableSet(concept)
      contexts.put(core, {
        val contextIndex = new RootContextClauseIndex(superConcepts.addKey(concept.iri))
        val edge = UnboundedChannel[InterContextMessage]()
        val ordering = RootContextLiteralOrdering
        val newContext: Proc = buildContext(core, contextIndex, edge, ordering)
        fork(newContext)
        edge
      })
    }
  }

  /**
    *
    * @param K1
    * @return
    */
  def getSuccessor(K1: ImmutableSet[Predicate]): UnboundedChannel[InterContextMessage] = synchronized {
    val core: ImmutableSet[Predicate] = strategy(K1)
    if (core.isEmpty) logger.warn(s"WARNING: trivial context is active! (K1 = $K1)")
    contexts.getOrElseUpdate(core, {
      val contextIndex = new ContextClauseIndex
      val edge = UnboundedChannel[InterContextMessage]()
      val ordering = NonRootContextLiteralOrdering
      val newContext: Proc = buildContext(core, contextIndex, edge, ordering)
      contextRoundStarted
      fork(newContext)
      edge
    })
  }

  def getTaxonomy: Taxonomy = {
    latch.await
    Taxonomy(superConcepts)
  }

}
