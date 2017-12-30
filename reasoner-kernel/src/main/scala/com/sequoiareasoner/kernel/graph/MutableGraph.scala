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

import scala.annotation.tailrec
import scala.collection.mutable

/** Implements a graph of reference nodes with unlabelled edges.
  *
  * This data structure is not thread safe.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @tparam T
  */
final class MutableGraph[T >: Null <: AnyRef] {
  private[this] val _nodes = new mutable.HashSet[T]
  private[this] val adjList = new mutable.HashMap[T, mutable.Set[T]] with mutable.MultiMap[T, T] {
    override def default(key: T) = mutable.Set.empty[T]
  }

  def addEdge(from: T, to: T): Unit = {
    adjList.addBinding(from, to)
    _nodes.add(from)
    _nodes.add(to)
  }

  def removeEdge(from: T, to: T): Unit = {
    // TODO: ensure that _nodes is also updated.
    adjList.removeBinding(from, to)
  }

  def edgeExists(from: T, to: T): Boolean = adjList(from).contains(to)

  def nodes: Set[T] = _nodes.toSet

  def getSuccessors(node: T): Set[T] = adjList(node).toSet ensuring {
    _  forall { _ != null }
  }

  def transitivelyClose: Unit = {
    val toProcess = new mutable.ArrayBuffer[T]
    for (reachable: mutable.Set[T] <- adjList.values) {
      toProcess.clear
      toProcess ++= reachable
      while (toProcess.nonEmpty) {
        val elementOnPath: T = toProcess.remove(toProcess.size - 1)
        val elementOnPathSuccessors: mutable.Set[T] = adjList(elementOnPath)
        for (elementOnPathSuccessor <- elementOnPathSuccessors)
          if (reachable.add(elementOnPathSuccessor))
            toProcess += elementOnPathSuccessor
      }
    }
  }

  def getInverse: MutableGraph[T] = {
    val result = new MutableGraph[T]
    for ((from, successor) <- adjList; to <- successor)
      result.addEdge(to, from)
    result
  }

	def removeNodes(elements: Set[T]): Unit =
		for(element <- elements){
			this._nodes -= element
			adjList.remove(element)
		}

  /** Compute the strongly-connected components (SCCs) of a digraph using Tarjan's algorithm. Returns the SCCs as a set
    * of equivalent classes.
    *
    * Runs in time proportional to `V + E` (in the worst case), where `V` is the number of vertices and `E` is the
    * number of edges.
    *
    * @return the strongly-connected components of this graph.
    */
  def stronglyConnectedComponents: AnyRefUnionFind[T] = {
    // marked(v) == true iff v been visited.
    val marked = new mutable.HashSet[T]
    // low(v) is the low number of v.
    val lowMap = new mutable.AnyRefMap[T, Int]
    @inline def low(v: T): Int = lowMap.getOrElse(v, 0)
    @inline def setLow(v: T, n: Int) = lowMap.put(v, n)
    // preorder number counter.
    var pre: Int = 0

    val stack = new mutable.ArrayStack[T]
    // The set of SCCs found so far.
    val components = new AnyRefUnionFind[T]

    @tailrec def buildComponent(root: T): Unit = {
      val w: T = stack.pop
      setLow(w, Int.MaxValue)
      components.union(root, w)
      if (w != root) buildComponent(root)
    }

    def dfs(v: T): Unit = {
      marked += v
      setLow(v, pre)
      pre += 1
      var min: Int = low(v)
      stack.push(v)
      for (w <- adjList(v)) {
        if (!marked(w)) dfs(w)
        if (low(w) < min) min = low(w)
      }
      if (min < low(v))
        setLow(v, min)
      else
        buildComponent(v)
    }

    for (v <- nodes) if (!marked(v)) dfs(v)

    components
  }

  override def toString: String = {
    val buffer = new StringBuilder
    for (element <- _nodes) {
      buffer.append(element)
      buffer.append(" -> { ")
      val successors = adjList(element)
      successors.addString(buffer, ", ")
      buffer.append(" }")
      buffer.append(System.lineSeparator)
    }
    buffer.result
  }

}
