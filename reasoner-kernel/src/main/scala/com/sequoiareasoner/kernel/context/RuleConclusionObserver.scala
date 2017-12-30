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
import com.sequoiareasoner.kernel.index.ImmutableSet

/** A trait allowing for the recording of rule conclusions. A method is provided for each rule of the calculus.
  * Note however, that because the Ineq rule should be applied eagerly after each rule application, there is not a
  * method for the Ineq rule here.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
trait RuleConclusionObserver {
  def core: ImmutableSet[Predicate]

  def coreConclusion(clause: ContextClause): Boolean

  def hyperConclusion(clause: ContextClause): Boolean

  def predConclusion(clause: ContextClause): Boolean

  def succConclusion(clause: ContextClause): Boolean

  def eqConclusion(clause: ContextClause): Boolean
}
