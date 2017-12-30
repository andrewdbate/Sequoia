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

import com.sequoiareasoner.kernel.clauses.Term

/** Implements a union-find data structure for terms.
  *
  * It supports the usual `union` and `find` operations, along with a method to determine whether two terms are in the
  * same component.
  *
  * This implementation uses weighted quick union (by size) with full path compression.
  *
  * The worst-case time complexity of the methods `union`, `find`, and `isConnected` is logarithmic.
  * The amortized time per `union`, `find`, and `isConnected` operation has inverse Ackermann complexity.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @constructor Initializes an empty union-find data structure. Each term is initially in its own component.
  */
class TermUnionFind {

  /** parent(i) = parent of i. */
  private[this] val parent: IntIntMap = IntIntMapImpl()

  private[this] def ensureInitialized(id: Int): Unit = parent.getOrElseUpdate(id, id)

  /** Returns the component identifier for the component containing the specified term `t`.
    *
    * @param t the term
    * @return the component identifier for the component containing term `t`
    */
  private[this] def find(t: Term): Int = {
    var p = t.id
    ensureInitialized(p)
    var root = p
    while (root != parent(root)) {
      root = parent(root)
    }
    while (p != root) {
      val pNew: Int = parent(p)
      parent(p) = root
      p = pNew
    }
    root
  }

  /** Returns `true` if the the two specified terms are in the same component.
    *
    * @param  p the first term
    * @param  q the second term
    * @return `true` if the two sites `p` and `q` are in the same component; `false` otherwise
    */
  def isConnected(p: Term, q: Term): Boolean = find(p) == find(q)

  /** Merges the component containing term `p` with the the component containing term `q`.
    *
    * @param  p the first term
    * @param  q the second term
    */
  def union(p: Term, q: Term): Unit = {
    val rootP: Int = find(p)
    val rootQ: Int = find(q)
    if (rootP != rootQ) {
      // Make the term with the smaller UID the root.
      if (rootP < rootQ) {
        parent(rootQ) = rootP
      } else {
        parent(rootP) = rootQ
      }
    }
  }

}
