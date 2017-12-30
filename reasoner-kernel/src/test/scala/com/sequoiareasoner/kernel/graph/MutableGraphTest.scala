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

package com.sequoiareasoner.kernel.graph

import com.sequoiareasoner.kernel.index.AnyRefUnionFind
import org.scalatest.FunSuite

class MutableGraphTest extends FunSuite {

  // TODO: Add randomised test cases.

  test("transitivity"){
    // Use Integer instead of Int because MutableGraph expects an AnyRef.
    val g = new MutableGraph[Integer]
    g.addEdge(0, 1)
    g.addEdge(1, 2)
    g.addEdge(2, 9)
    g.addEdge(9, 8)
    g.addEdge(8, 7)
    g.addEdge(7, 6)
    g.transitivelyClose

    assert(Set(1, 2, 6, 7, 8, 9) === g.getSuccessors(0))
    assert(Set(   2, 6, 7, 8, 9) === g.getSuccessors(1))
    assert(Set(      6, 7, 8, 9) === g.getSuccessors(2))
    assert(Set(      6, 7, 8   ) === g.getSuccessors(9))
    assert(Set(      6, 7      ) === g.getSuccessors(8))
    assert(Set(      6         ) === g.getSuccessors(7))
  }

  ignore("transitivity (large graph)"){
    val g = new MutableGraph[Integer]
    g.addEdge( 4,  2)
    g.addEdge( 2,  3)
    g.addEdge( 3,  2)
    g.addEdge( 6,  0)
    g.addEdge( 0,  1)
    g.addEdge( 2,  0)
    g.addEdge(11, 12)
    g.addEdge(12,  9)
    g.addEdge( 9, 10)
    g.addEdge( 9, 11)
    g.addEdge( 7,  9)
    g.addEdge(10, 12)
    g.addEdge(11,  4)
    g.addEdge( 4,  3)
    g.addEdge( 3,  5)
    g.addEdge( 6,  8)
    g.addEdge( 8,  6)
    g.addEdge( 5,  4)
    g.addEdge( 0,  5)
    g.addEdge( 6,  4)
    g.addEdge( 6,  9)
    g.addEdge( 7,  6)
    val expectedNodes: Set[Integer] = Set(0, 1, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9)
    val actualNodes: Set[Integer] = g.nodes
    assert(actualNodes === expectedNodes)
    g.transitivelyClose

    ???
  }

  test("inverse"){
    val g = new MutableGraph[Integer]
    g.addEdge(0, 1)
    g.addEdge(1, 2)
    g.addEdge(2, 9)
    g.addEdge(9, 8)
    g.addEdge(8, 7)
    g.addEdge(7, 6)

    val invG = g.getInverse

    assert(invG.getSuccessors(0) === Set( ))
    assert(invG.getSuccessors(1) === Set(0))
    assert(invG.getSuccessors(2) === Set(1))
    assert(invG.getSuccessors(9) === Set(2))
    assert(invG.getSuccessors(8) === Set(9))
    assert(invG.getSuccessors(7) === Set(8))
    assert(invG.getSuccessors(6) === Set(7))
  }

  test("remove"){
    val g = new MutableGraph[Integer]
    g.addEdge(0, 1)
    g.addEdge(1, 2)
    g.addEdge(2, 9)
    g.addEdge(9, 8)
    g.addEdge(8, 7)
    g.addEdge(7, 6)
    assert(g.nodes === Set(0, 1, 2, 9, 8, 7, 6))
    g.removeNodes(Set(2, 8))
    g.removeNodes(Set(9))
    assert(g.nodes === Set(0, 1, 7, 6))
    g.removeNodes(Set(6))
    assert(g.nodes === Set(0, 1, 7))
    assert(g.getSuccessors(0) === Set(1))
    assert(g.getSuccessors(1) === Set(2))
    assert(g.getSuccessors(2) === Set( ))
    assert(g.getSuccessors(9) === Set( ))
    assert(g.getSuccessors(8) === Set( ))
    assert(g.getSuccessors(7) === Set(6))
    assert(g.getSuccessors(6) === Set( ))
  }

  test("scc") {
    val g = new MutableGraph[Integer]
    g.addEdge( 4,  2)
    g.addEdge( 2,  3)
    g.addEdge( 3,  2)
    g.addEdge( 6,  0)
    g.addEdge( 0,  1)
    g.addEdge( 2,  0)
    g.addEdge(11, 12)
    g.addEdge(12,  9)
    g.addEdge( 9, 10)
    g.addEdge( 9, 11)
    g.addEdge( 7,  9)
    g.addEdge(10, 12)
    g.addEdge(11,  4)
    g.addEdge( 4,  3)
    g.addEdge( 3,  5)
    g.addEdge( 6,  8)
    g.addEdge( 8,  6)
    g.addEdge( 5,  4)
    g.addEdge( 0,  5)
    g.addEdge( 6,  4)
    g.addEdge( 6,  9)
    g.addEdge( 7,  6)
    val expectedNodes: Set[Integer] = Set(0, 1, 10, 11, 12, 2, 3, 4, 5, 6, 7, 8, 9)
    val actualNodes: Set[Integer] = g.nodes
    assert(actualNodes === expectedNodes)
    val scc: AnyRefUnionFind[Integer] = g.stronglyConnectedComponents
    /* We expect the components to be the following:
     * 1
     * 0 2 3 4 5
     * 9 10 11 12
     * 6 8
     * 7
     */
    val expectedComponents = Map(
      1  -> Set(1),
      0  -> Set(0, 2, 3, 4, 5),
      2  -> Set(0, 2, 3, 4, 5),
      3  -> Set(0, 2, 3, 4, 5),
      4  -> Set(0, 2, 3, 4, 5),
      5  -> Set(0, 2, 3, 4, 5),
      9  -> Set(9, 10, 11, 12),
      10 -> Set(9, 10, 11, 12),
      11 -> Set(9, 10, 11, 12),
      12 -> Set(9, 10, 11, 12),
      6  -> Set(6, 8),
      8  -> Set(6, 8),
      7  -> Set(7)
    )
    for (l <- expectedNodes; r <- expectedNodes) {
      val sameComponent = expectedComponents(l) contains r
      if (sameComponent)
        assert(scc.samePartition(l, r), s"$l and $r should have been in the same component.")
      else
        assert(!scc.samePartition(l, r), s"$l and $r should have been in different components.")
    }
  }

}
