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

package com.sequoiareasoner.kernel.index

import com.sequoiareasoner.kernel.clauses.{ContextClause, Literal, PredClause, Predicate}
import com.sequoiareasoner.kernel.owl.iri.IRI

/** An object of functions for instantiating arrays of a particular type and length. These functions are used
  * in collections backed by arrays for which the runtime type of the array is required.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object ArrayBuilders {

  final val contextClauseArrayBuilder: Int => Array[ContextClause] = new Array[ContextClause](_)

  final val predicateArrayBuilder: Int => Array[Predicate] = new Array[Predicate](_)

  final val emptyPredicateArray = new Array[Predicate](0)

  final val literalArrayBuilder: Int => Array[Literal] = new Array[Literal](_)

  final val emptyLiteralArray = new Array[Literal](0)

  final val iriArrayBuilder: Int => Array[IRI] = new Array[IRI](_)

  final val predClauseArrayBuilder: Int => Array[PredClause] = new Array[PredClause](_)

}
