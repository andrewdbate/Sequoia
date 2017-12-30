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

import scala.collection.mutable

object AnyRefUnionFind {

  private final class Partition[A <: AnyRef](val elem: A) {
    private var parent: Partition[A] = this
    private var rank: Int = 0

    def findRoot: Partition[A] = {
      var current: Partition[A] = this
      var next: Partition[A] = current.parent
      while (next ne current) {
        current.parent = next.parent
        current = next
        next = next.parent
      }
      current
    }

    def union(p: Partition[A]): Boolean = {
      val rep: Partition[A] = findRoot
      val pRep: Partition[A] = p.findRoot
      if (rep eq pRep) {
        return false
      } else if (rep.rank < pRep.rank) {
        rep.parent = pRep
      } else if (rep.rank > pRep.rank) {
        pRep.parent = rep
      } else {
        rep.parent = pRep
        pRep.rank += 1
      }
      return true
    }

  }

}

/** A partitioning of a fixed set of elements into disjoint sets which can be efficiently merged.
  *
  * This data structure is not thread safe.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
final class AnyRefUnionFind[A <: AnyRef] {
  import AnyRefUnionFind._

  private[this] val map = new mutable.AnyRefMap[A, Partition[A]]

  private[this] def get(elem: A): Partition[A] = {
    if (elem eq null) throw new NullPointerException
    map.getOrElseUpdate(elem, new Partition(elem))
  }

  def getRepresentative(element: A): A = {
    val partition: Partition[A] = get(element)
    partition.findRoot.elem
  }

  def union(a: A, b: A): Unit = {
    val aPart: Partition[A] = get(a)
    val bPart: Partition[A] = get(b)
    aPart union bPart
  }

  def samePartition(a: A, b: A): Boolean = {
    val aPart: Partition[A] = get(a)
    val bPart: Partition[A] = get(b)
    aPart.findRoot eq bPart.findRoot
  }

  override def toString: String = map.keys.groupBy{ map(_).findRoot }.values.mkString("[", ",", "]")

}
