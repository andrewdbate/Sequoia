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

import com.sequoiareasoner.kernel.clauses.ContextClause

object TrieContextClauseRedundancyIndex {

  private def clauseToKey(clause: ContextClause): Array[Long] = {
    val bodyLength = clause.body.length
    val headLength = clause.head.length
    val keyLength = bodyLength + headLength
    val key = new Array[Long](keyLength)
    var i = 0
    while (i < headLength) {
      key(i) = Long.MinValue | clause.head(i).uid
      i += 1
    }
    while (i < keyLength) {
      key(i) = clause.body(i - headLength).uid
      i += 1
    }
    // TODO: The body and head should be already sorted by uid.
    java.util.Arrays.sort(key, 0, headLength)
    java.util.Arrays.sort(key, headLength, keyLength)
    key
  }

}

/** An implementation of a context clause redundancy index that uses a trie of body and head literals to implement
  * forward redundancy checking and backward redundancy elimination.
  *
  * @author Andrew Bate <code@andrewbate>
  */
final class TrieContextClauseRedundancyIndex extends ContextClauseRedundancyIndex {
  import TrieContextClauseRedundancyIndex.clauseToKey

  private[this] val trie = new TrieSearchTree[ContextClause]

  override def add(clause: ContextClause): Unit = {
    val seq = clauseToKey(clause)
    trie.putIfAbsent(seq, clause)
  }

  override def isClauseSubsumed(clause: ContextClause): Boolean = {
    val seq = clauseToKey(clause)
    trie.containsKeySubset(seq)

  }

  override def isClauseStrictlySubsumed(clause: ContextClause): Boolean = {
    val seq = clauseToKey(clause)
    trie.containsKeyStrictSubset(seq)
  }

  override def removeSubsumedClauses[U](clause: ContextClause)(f: ContextClause => Unit): Unit = {
    val seq = clauseToKey(clause)
    trie.removeKeySuperset(seq)(f)
  }

  override def toString = trie.addString(new StringBuilder, "TrieContextClauseRedundancyIndex[\n", "\n", "\n]").result

}
