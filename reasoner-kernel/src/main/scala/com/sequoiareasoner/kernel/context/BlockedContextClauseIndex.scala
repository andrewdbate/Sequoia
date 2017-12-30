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

import com.sequoiareasoner.kernel.clauses.{ContextClause, Predicate}
import com.sequoiareasoner.kernel.index.{ArrayBuilders, ArrayIndexedSequence, TrieSearchTree}

object BlockedContextClauseIndex {

  private def bodyToKey(body: Array[Predicate]): Array[Long] = {
    val bodyLength = body.length
    val keyLength = bodyLength
    val key = new Array[Long](keyLength)
    var i = 0
    while (i < keyLength) {
      key(i) = body(i).uid
      i += 1
    }
    // TODO: The body should be already sorted by uid.
    java.util.Arrays.sort(key, 0, bodyLength)
    key
  }

}

/** A collection of clauses that are derived in a context, but are not relevant for any predecessor.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
final class BlockedContextClauseIndex {
  import BlockedContextClauseIndex._

  private[this] val blockedClauseTrie = new TrieSearchTree[ArrayIndexedSequence[ContextClause]]

  /** Moves clauses from this blocked clauses collection into the queue of unprocessed clauses.
    *
    * @param bodySuperset
    * @param f
    */
  def retrieveAndRemoveClauses(bodySuperset: Array[Predicate], f: ContextClause => Unit): Unit = {
    val keySuperset = bodyToKey(bodySuperset)
    blockedClauseTrie.removeKeySubset(keySuperset){
      clauses: ArrayIndexedSequence[ContextClause] => clauses.foreach(f)
    }
  }

  def add(clause: ContextClause): Unit = {
    val keySuperset = bodyToKey(clause.body)
    val values: ArrayIndexedSequence[ContextClause] =
      blockedClauseTrie.getOrElseUpdate(keySuperset, new ArrayIndexedSequence[ContextClause](4, ArrayBuilders.contextClauseArrayBuilder))
    values += clause
  }

}
