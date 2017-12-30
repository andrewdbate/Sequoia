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

package com.sequoiareasoner.kernel.logging

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.index.ImmutableSet

/** This object can be used observe when rules of the calculus are applied. This object is thread-safe.
  *
  * It it intended to be used for debugging only.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object DerivationObserver {

  private[this] final val debug = false
  private[this] final val toFile = false

  private[this] lazy val writer: PrintWriter =
    if (toFile)
      new PrintWriter(new BufferedWriter(new FileWriter(s"trace-${System.currentTimeMillis}.txt", true)))
    else
      new PrintWriter(System.err, true) // Use auto flushing.

  type Core = ImmutableSet[Predicate]

  private[this] def log(core: Core)(message: String*): Unit =
    writer.println(s"${core.mkString("[", ",", "]")}\n${message.mkString("    ","\n    ","\n")}")

  /** Called when the Core rule is applied within some context.
    *
    * @param core the core of the context within which the Core rule was applied.
    * @param p the predicate such that `\top -> p` is in the strengthening after the rule is applied.
    */
  def coreFired(core: Core, p: Predicate): Unit = if (debug) synchronized {
    log(core)(s"[core] T -> $p")
  }

  /** Called when the Hyper rule is applied within some context.
    *
    * @param core the core of the context within which the Hyper rule was applied.
    * @param c
    * @param sigma
    * @param cs
    * @param conclusion
    */
  def hyperFired(core: Core, c: OntologyClause, sigma: Substitution, cs: Seq[ContextClause], conclusion: ContextClause): Unit = if (debug) synchronized {
    val lines = s"[hyper] $c" +:
      (cs ++ Seq(s"sigma = $sigma", s"--> $conclusion") map {"        " + _})
    log(core)(lines: _*)
  }

  /** Called when the Eq rule is applied within some context.
    *
    * @param core the core of the context within which the Eq rule was applied.
    * @param c1
    * @param c2
    * @param conclusion
    */
  def eqFired(core: Core, c1: ContextClause, c2: ContextClause, conclusion: ContextClause): Unit = if (debug) synchronized {
    log(core)(s"[eq] ")
  }

  /** Called when the Ineq rule is applied within some context.
    *
    * @param core the core of the context within which the Ineq rule was applied.
    * @param c1
    * @param conclusion
    */
  def ineqFired(core: Core, c1: ContextClause, conclusion: ContextClause): Unit = if (debug) synchronized {
    log(core)(s"[ineq] ")
  }

  /** Called when the Factor rule is applied within some context.
    *
    * @param core the core of the context within which the Factor rule was applied.
    * @param c1
    * @param conclusion
    */
  def factorFired(core: Core, c1: ContextClause, conclusion: ContextClause): Unit = if (debug) synchronized {
    log(core)(s"[factor] ")
  }

  /** Called when the Elim rule is applied within some context.
    *
    * @param core the core of the context within which the Elim rule was applied.
    * @param eliminated the clause that was removed by the Elim rule.
    */
  def elimFired(core: Core, eliminated: ContextClause): Unit = if (debug) synchronized {
    log(core)(s"[elim] ")
  }

  /** Called when the Pred rule is applied within some context.
    *
    * @param predecessor the predecessor context to which a conclusion is propagated from the successor.
    * @param successor the context from which the conclusion propagates.
    */
  def predFired(predecessor: Core, successor: Core): Unit = if (debug) synchronized {
    log(predecessor)(s"[pred] ")
  }

  /** Called when the Succ rule is applied within some context.
    *
    * @param predecessor the core of the predecessor context from which conclusions are propagated to the successor.
    * @param successor the core of the context to which the conclusion propagate.
    */
  def succFired(predecessor: Core, neighbour: Term, successor: Core, isExistingNeighbour: Boolean): Unit = if (debug) synchronized {
    log(predecessor)(s"[succ] neighbour = $neighbour ${if (isExistingNeighbour) "(existing neighbour)" else "(new neighbour)"}",
                     s"       successor = $successor")
  }

}
