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

package com.sequoiareasoner.kernel.index

import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.owl.iri.IRI

/** Maintains an index of the clauses that can participate in the Pred rule for a single context and that have been
  * pushed back from successor context.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class NeighborPredClauseIndex {

  private[this] val conceptIndex = new LongIndexedSequenceMap[PredClause](8, ArrayBuilders.predClauseArrayBuilder)
  private[this] val forwardsRoleIndex = new LongIndexedSequenceMap[PredClause](8, ArrayBuilders.predClauseArrayBuilder)
  private[this] val backwardsRoleIndex = new LongIndexedSequenceMap[PredClause](8, ArrayBuilders.predClauseArrayBuilder)

  private[this] def compoundKey(iri: IRI, t: Term): Long = iri.uid.toLong << 32 | t.id.toLong

  import com.sequoiareasoner.arrayops._

  def add(t: Term, neighbourCore: ImmutableSet[Predicate], c: ContextClause): PredClause = {
    require(t.isFunctionTerm)
    val sigma = new BackwardsInterContextSubstitution(t)
    val neighbourCoreSeq: Seq[Predicate] = neighbourCore.toSeq
    val clauseBodyLength: Int = c.body.length
    val newBody: Array[Predicate] = new Array[Predicate](clauseBodyLength + neighbourCore.size)
    crange(0, newBody.length) { i =>
      newBody(i) =
        if (i < clauseBodyLength) c.body(i).applySubstitution(sigma)
        else neighbourCoreSeq(i - clauseBodyLength).applySubstitution(sigma)
    }
    val newHead: Array[Predicate] = cmap (c.head) {_.applySubstitution(sigma).asInstanceOf[Predicate]}
    val predClause: PredClause = PredClause(newBody, newHead)
    cforeach (newBody) {
      case Concept(iri, s) if s.isFunctionTerm =>
        conceptIndex.addBinding(compoundKey(iri, s), predClause)
      case Role(iri, s1, s2) if s1.isCentralVariable && s2.isFunctionTerm =>
        forwardsRoleIndex.addBinding(compoundKey(iri, s2), predClause)
      case Role(iri, s1, s2) if s1.isFunctionTerm && s2.isCentralVariable =>
        backwardsRoleIndex.addBinding(compoundKey(iri, s1), predClause)
    }
    predClause
  }

  def apply(p: Predicate): IndexedSequence[PredClause] =
    p match {
      case Concept(iri, t) if t.isFunctionTerm =>
        conceptIndex(compoundKey(iri, t))
      case Role(iri, t1, t2) if t1.isCentralVariable && t2.isFunctionTerm =>
        forwardsRoleIndex(compoundKey(iri, t2))
      case Role(iri, t1, t2) if t1.isFunctionTerm && t2.isCentralVariable =>
        backwardsRoleIndex(compoundKey(iri, t1))
    }

  override def toString: String = {
    val builder = new StringBuilder
    builder.append("NeighborPredClauseIndex[\n")
    def buildString(k: Long, vs: IndexedSequence[PredClause]): Unit =
      vs.addString(builder.append(k), " -> [", ", ", "]\n")
    conceptIndex foreach buildString
    forwardsRoleIndex foreach buildString
    backwardsRoleIndex foreach buildString
    builder.append("]")
    builder.result
  }

}
