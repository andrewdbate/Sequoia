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

import com.sequoiareasoner.kernel.index.{IntIntMap, IntIntMapImpl}
import com.sequoiareasoner.arrayops.cmap

import Term._

/** The type of all substitutions.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
sealed trait Substitution {
  self =>
  def add(i: Term, o: Term): Boolean
  /** Apply this substitution to a term, or throws an exception if `v` is not in the domain of the substitution.
    *
    * @param v  the input term
    * @return the term `v` with the substitution applied
    */
  def apply(v: Term): Term
  final def apply(in: Array[Literal]): Array[Literal] = cmap (in) { _.applySubstitution(self) }
}

/** Given a term `fx`, this class provides the term-to-term mapping {`fx` -> x, x -> y}.
  * This is not a technically a substitution in the usual sense, since the domain of the mapping is not the variables,
  * but it does describe the relationship between terms moving from a predecessor context to a successor context.
  *
  * @param fx
  */
final class ForwardsInterContextMapping(fx: Term) extends Substitution {
  require(fx.isFunctionTerm)
  override def add(i: Term, o: Term): Boolean =
    (i == fx && o == x) || (i == x && o == y)
  override def apply(v: Term): Term =
    if (v == fx) x
    else if (v == x) y
    else throw new IllegalArgumentException(s"$v is neither $fx nor $x")
  override def toString: String = s"Succ {$fx -> $x, $x -> $y}"
}

/** Given a term `fx`, this class provides the substitution {x -> `fx`, y -> x}.
  *
  * @param fx
  */
final class BackwardsInterContextSubstitution(fx: Term) extends Substitution {
  require(fx.isFunctionTerm)
  override def add(i: Term, o: Term): Boolean =
    (i == y && o == x) || (i == x && o == fx)
  override def apply(v: Term): Term =
    if (v == y) x
    else if (v == x) fx
    else throw new IllegalArgumentException(s"$v is neither $x nor $y")
  override def toString: String = s"Pred {$x -> $fx, $y -> $x}"
}

final class CentralSubstitution private (s: IntIntMap) extends Substitution {
  def this() = this(IntIntMapImpl())

  override def add(i: Term, o: Term): Boolean = {
    require(i.isVariable)
    if (i == x) o == x
    else s.getOrElseUpdate(i.id, o.id) == o.id
  }

  /** Apply this substitution to a term. (Note that x and f(x) will always map to x and f(x), respectively.)
    *
    * Throws a [[NoSuchElementException]] is the term is not in the domain of the substitution.
    *
    * @param v
    * @return
    */
  override def apply(v: Term): Term =
    if (v == x || v.isFunctionTerm) v
    else Term(s(v.id))

  override def toString: String = if (s.isEmpty) s"Central {$x -> $x}" else s"Central {$x -> $x, ${s mkString ", "}}"
}

/** A substitution to swap the variables `x` and `v`.
  *
  * @constructor
  * @param v
  */
final class VariableSwapSubstitution(v: Term) extends Substitution {
  require(v.isVariable, s"$v is not a variable.")

  override def add(i: Term, o: Term): Boolean =
    (i == x && o == v) || (i == v && o == x)

  /** Apply the substitution to a term from the input ontology.
    *
    * @param i
    * @return
    */
  override def apply(i: Term): Term =
    if (i == x) v
    else if (i == v) x
    else if (v == x && i.isFunctionTerm) i
    else throw new IllegalArgumentException(s"$i is neither $x nor $v")

  override def toString: String = s"Swap {$x -> $v, $v -> $x}"
}

/** Given a term `t`, this class provides the substitution {`x` -> `t`}.
  *
  * @param t
  */
final class TermSubstitution(t: Term) extends Substitution {

  override def add(i: Term, o: Term): Boolean = i == x && o == t

  /** Apply the substitution to a term from the input ontology.
    *
    * @param i
    * @return
    */
  override def apply(i: Term): Term =
    if (i == x) t
    else throw new IllegalArgumentException(s"$i is not $x. (Cannot apply $toString.)")

  override def toString: String = s"Term {$x -> $t}"

}
