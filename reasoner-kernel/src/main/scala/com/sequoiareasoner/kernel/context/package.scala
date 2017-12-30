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

package com.sequoiareasoner.kernel

import com.sequoiareasoner.kernel.clauses.{ContextClause, Predicate, Term}
import com.sequoiareasoner.kernel.index.{ImmutableSet, IndexedSequence}
import io.cso.UnboundedChannel

/** Package containing the types of messages that can be exchanged between communicating contexts for the
  * implementation of the Pred and Succ rules.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
package object context {

  /**
    * Marker trait for inter-context messages sent during the application of the Pred and Succ rules of the calculus.
    */
  sealed trait InterContextMessage extends Any

  /** A message to notify a context that it must apply the Pred rule using the supplied information.
    *
    * @param edgeLabel
    * @param core
    * @param clauses
    */
  final case class PredPush(edgeLabel: Term,
                            core: ImmutableSet[Predicate],
                            clauses: IndexedSequence[ContextClause]) extends InterContextMessage

  /** A message to notify a context that it must apply the Succ rule using the supplied information.
    *
    * @param contextChannel
    * @param edgeLabel
    * @param predicate
    */
  final case class SuccPush(contextChannel: UnboundedChannel[InterContextMessage],
                            edgeLabel: Term,
                            predicate: Predicate) extends InterContextMessage

}
