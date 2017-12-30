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

package com.sequoiareasoner.kernel.logging

import com.sequoiareasoner.kernel.clauses.ContextClause
import com.sequoiareasoner.kernel.context.RuleConclusionObserver

/** A logging mixin for [[RuleConclusionObserver]] implementations.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
trait RuleConclusionLogger extends RuleConclusionObserver {

  protected[this] def loggingEnabled: Boolean = true

  abstract override def coreConclusion(clause: ContextClause): Boolean = {
    val result = super.coreConclusion(clause)
    if (loggingEnabled && result) System.err.println(s"${core.mkString("[",", ","]")}: CORE  $clause")
    result
  }

  abstract override def hyperConclusion(clause: ContextClause): Boolean = {
    val result = super.hyperConclusion(clause)
    if (loggingEnabled && result) System.err.println(s"${core.mkString("[",", ","]")}: HYPER $clause")
    result
  }

  abstract override def predConclusion(clause: ContextClause): Boolean = {
    val result = super.predConclusion(clause)
    if (loggingEnabled && result) System.err.println(s"${core.mkString("[",", ","]")}: PRED  $clause")
    result
  }

  abstract override def succConclusion(clause: ContextClause): Boolean = {
    val result = super.succConclusion(clause)
    if (loggingEnabled && result) System.err.println(s"${core.mkString("[",", ","]")}: SUCC  $clause")
    result
  }

  abstract override def eqConclusion(clause: ContextClause): Boolean = {
    val result = super.eqConclusion(clause)
    if (loggingEnabled && result) System.err.println(s"${core.mkString("[",", ","]")}: EQ    $clause")
    result
  }

}
