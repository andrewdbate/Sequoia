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

package com.sequoiareasoner.kernel.clauses

/** Class representing a term, which can either be the central variable `x`, the predecessor variable `y`,
  * a neighbour variable of the form `z_i`m or a successor term of the form `f_i(x)`,
  *
  * To avoid the overhead of using final case classes to represent variables and terms,
  * we instead using the following convention throughout the code:
  *  - variable x is represented as zero
  *  - variable z (in context clauses) and z_1 (in ontology clauses) is represented by -1
  *  - variable z_i is represented by -i
  *  - the term f_i(x) is represented by +1
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class Term private (val id: Int) extends AnyVal {
  // Avoid static method calls for small operations by inlining these methods.
  @inline def isCentralVariable: Boolean = id == 0
  @inline def isPredecessorVariable: Boolean = id == -1
  @inline def isNeighbourVariable: Boolean = id < 0
  @inline def isVariable: Boolean = id <= 0
  @inline def isFunctionTerm: Boolean = id > 0
  @inline def max(that: Term): Term = if (this.id < that.id) new Term(that.id) else new Term(this.id)
  @inline def min(that: Term): Term = if (this.id < that.id) new Term(this.id) else new Term(that.id)
  @inline def <(that: Term): Boolean = this.id < that.id
  @inline def <=(that: Term): Boolean = this.id <= that.id
  @inline def >(that: Term): Boolean = this.id > that.id

  override def toString: String =
    if (isCentralVariable) "x"
    else if (isPredecessorVariable) "y"
    else if (isNeighbourVariable) s"z${-1*id - 1}"
    else s"f$id(x)"

  /**
    * @param that
    * @return a negative integer, zero, or a positive integer if `this` is less than, equal to, or greater than `that`.
    */
  def compare(that: Term): Int = this.id - that.id
}

object Term {
  // No explicit type due to weirdness in the language specification Scala 2.12!
  final val x = new Term(0)
  final val y = new Term(-1)
  // Ensure that y and z_1 do not map to the same term.
  def z(i: Int): Term = {
    require(i > 0)
    new Term(-i-1)
  }
  def f(i: Int): Term = {
    require(i > 0)
    new Term(i)
  }
  def apply(id: Int): Term = new Term(id)
}
