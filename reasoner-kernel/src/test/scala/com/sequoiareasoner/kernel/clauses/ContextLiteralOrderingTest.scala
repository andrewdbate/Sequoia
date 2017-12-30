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

import com.sequoiareasoner.kernel.CommonNames
import org.scalatest.FunSuite
import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.structural.{DLOntology, UnsupportedFeatureObserverThrowException}

class ContextLiteralOrderingTest extends FunSuite {
  import CommonNames._

  // TODO: implement property based testing.

  // Dummy implicit ontology to allow for construction of Concepts and Roles.
  private[this] implicit val ontology = new DLOntology(Set.empty, new UnsupportedFeatureObserverThrowException)

  private[this] val Af1x = Concept(A, f1)
  private[this] val Af2x = Concept(A, f2)
  private[this] val Ax = Concept(A, x)
  private[this] val Ay = Concept(A, y)
  private[this] val Bx = Concept(B, x)
  private[this] val By = Concept(B, y)

  // Check that we are following the definitions for ordering on equalities x == y.
  private[this] val xEQy = Equality(x, y)
  private[this] val f1xEQy = Equality(f1, y)
  private[this] val f1xINEQy = Inequality(f1, y)
  private[this] val f2xEQf1x = Equality(f2, f1)
  private[this] val f3xEQf1x = Equality(f3, f1)

  private[this] val disjunctAux1 = Concept(IRI.disjunct(), x)
  private[this] val disjunctAux2 = Concept(IRI.disjunct(), x)

  private[this] val allAux1 = Concept(IRI.all(), x)
  private[this] val allAux2 = Concept(IRI.all(), x)

  /** An implicit class for convenience to allow one to write `literal1 << literal2` or `literal1 <<= literal2`
    * in the tests instead of the less readable `order.lt(l, other)` or `order.lteq(l, other)`.
    */
  private[this] implicit class RichLiteral(l: Literal)(implicit order: ContextLiteralOrdering) {
    def <<(other: Literal): Boolean = order.lt(l, other)
    def <<=(other: Literal): Boolean = order.lteq(l, other)
    override val toString: String = l.toString // Pass through for printing of errors.
  }

  test ("literal order respects context order requirements (non-root case)") {
    implicit val order = new ContextLiteralOrdering(root = false)

    assert(Ax << Bx)
    assert(xEQy << Ax)
    assert(Ax << f1xEQy)
    assert(f1xEQy << f1xINEQy)
    assert(!(f1xINEQy <<= f1xEQy))
    assert(Ay <<= f1xEQy)
    assert(Af1x << f2xEQf1x)
    assert(f2xEQf1x <<= Af2x)
    assert(f1 < f2)
    // The next ordering constant is not required to satisfy the definitions of a context order, but is crucial for performance.
    assert(Bx <<= Af1x)
    assert(Af1x <<= Af2x)
    assert(Ax <<= Af1x)
    assert(Ax <<= Af1x)
    assert(Ay <<= Ax)
    assert(f2xEQf1x <<= f3xEQf1x)
    // Check that the condition on pred triggers holds.
    assert(!(Ay <<= By))
    assert(!(By <<= Ay))
    // Check that auxiliaries introduced for disjunctions have an order over them.
    assert(disjunctAux1 <<= disjunctAux2)
    assert(!(disjunctAux2 <<= disjunctAux1))
    assert(allAux1 <<= allAux2)
  }

  test ("literal order respects context order requirements (root case)") {
    implicit val order = new ContextLiteralOrdering(root = true)

    // The following two cases differ from the non-root case.
    assert(!(Ax << Bx))
    assert(!(Bx << Ax))

    assert(xEQy << Ax)
    assert(Ax << f1xEQy)
    assert(f1xEQy << f1xINEQy)
    assert(!(f1xINEQy <<= f1xEQy))
    assert(Ay <<= f1xEQy)
    assert(Af1x << f2xEQf1x)
    assert(f2xEQf1x <<= Af2x)
    assert(f1 < f2)
    // The next ordering constant is not required to satisfy the definitions of a context order, but is crucial for performance.
    assert(Bx <<= Af1x)
    assert(Af1x <<= Af2x)
    assert(Ax <<= Af1x)
    assert(Ax <<= Af1x)
    assert(Ay <<= Ax)
    assert(f2xEQf1x <<= f3xEQf1x)
    // Check that the condition on pred triggers holds.
    assert(!(Ay <<= By))
    assert(!(By <<= Ay))
    // Check that auxiliaries introduced for disjunctions have an order over them.
    assert(disjunctAux1 <<= disjunctAux2)
    assert(!(disjunctAux2 <<= disjunctAux1))
    assert(allAux1 <<= allAux2)
  }

}
