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

package com.sequoiareasoner.kernel.context

import com.sequoiareasoner.kernel.{ClauseSetEquivalenceUtilities, CommonNames}
import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.index.{IndexedSequence, UnprocessedDeque}
import com.sequoiareasoner.kernel.structural.{DLOntology, UnsupportedFeatureObserverThrowException}
import org.scalatest.FunSuite

class HyperResolutionTest extends FunSuite {
  import ClauseSetEquivalenceUtilities._
  import CommonNames._

  // Passes the current suite implicitly to the decorated sets.
  private[this] implicit val self = this

  private[this] implicit val ontology = new DLOntology(Set.empty, new UnsupportedFeatureObserverThrowException)

  val Ax = Concept(A, x)
  val Bx = Concept(B, x)
  val Cx = Concept(C, x)
  val Cz = Concept(C, z1)
  val Cf = Concept(C, f1)
  val C1x = Concept(C1, x)
  val C2x = Concept(C2, x)
  val C3x = Concept(C3, x)
  val C4x = Concept(C4, x)
  val Dx = Concept(D, x)
  val Xx = Concept(X, x)
  val Yx = Concept(Y, x)
  val Y1x = Concept(Y1, x)
  val Y2x = Concept(Y2, x)
  val Zx = Concept(Z, x)
  val Rxz1 = Role(R, x, z1)
  val Rxz2 = Role(R, x, z2)
  val Rxf1 = Role(R, x, f1)
  val Rxf2 = Role(R, x, f2)
  val Sxz = Role(S, x, z1)
  val Rxy = Role(R, x, y)
  val Sxy = Role(S, x, y)

  def Predicates(ps: Predicate*): Array[Predicate] = ps.toArray

  private[this] val cutting: EqualityOptimization = EqualityOptimizationDisabled

  private[this] def Clause(body: Array[Predicate], head: Array[Literal]) = ContextClause(body, head)(RootContextLiteralOrdering)

  // TODO: use the same test, but parametrise with test data.

  private[this] def isNothing(l: Literal): Boolean = false

  test("Hyper-resolution 1") {
    val ontologyClause = OntologyClause(Body(Ax, Bx, Cx), Head(Dx))
    val workedOffContextClauses = Map[Predicate, IndexedSequence[ContextClause]](
      Bx -> IndexedSequence(Clause(Body(Yx), Head(Bx))),
      Cx -> IndexedSequence(Clause(Body(Zx), Head(Cx)))
    )
    val max = Ax
    val unprocessedClause = Clause(Body(Xx), Head(Ax))
    val expected = Set(
      Clause(Body(Xx, Yx, Zx), Head(Dx))
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }


  test("Hyper-resolution 2") {
    val ontologyClause = OntologyClause(Body(Ax, Bx, Cx), Head(Dx))
    val workedOffContextClauses = Map[Predicate, IndexedSequence[ContextClause]](
      Bx -> IndexedSequence(Clause(Body(Y1x), Head(Bx)), Clause(Body(Y2x), Head(Bx))),
      Cx -> IndexedSequence(Clause(Body(Zx), Head(Cx)))
    )
    val max = Ax
    val unprocessedClause = Clause(Body(Xx), Head(Ax))
    val expected = Set(
      Clause(Body(Xx, Y1x, Zx), Head(Dx)),
      Clause(Body(Xx, Y2x, Zx), Head(Dx))
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }

  test("Hyper-resolution 3") {
    val ontologyClause = OntologyClause(Body(Ax, Bx, Cx), Head(Dx))
    val workedOffContextClauses = Map[Predicate, IndexedSequence[ContextClause]](
      Ax -> IndexedSequence(Clause(Body(Xx), Head(Ax, C1x))),
      Bx -> IndexedSequence(Clause(Body(Y1x), Head(C2x, Bx)), Clause(Body(Y2x), Head(Bx, C3x)))
    )
    val max = Cx
    val unprocessedClause = Clause(Body(Zx), Head(C4x, Cx))
    val expected = Set(
      Clause(Body(Xx, Y1x, Zx), Head(Dx, C1x, C2x, C4x)),
      Clause(Body(Xx, Y2x, Zx), Head(Dx, C1x, C3x, C4x))
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }

  test("Hyper-resolution 4") {
    val ontologyClause = OntologyClause(Body(Ax, Bx, Cx), Head())
    val workedOffContextClauses = Map[Predicate, IndexedSequence[ContextClause]](
      Ax -> IndexedSequence(Clause(Body(Xx), Head(Ax, C1x))),
      Bx -> IndexedSequence(Clause(Body(Y1x), Head(C2x, Bx)), Clause(Body(Y2x), Head(Bx, C3x)))
    )
    val max = Cx
    val unprocessedClause = Clause(Body(Zx), Head(C4x, Cx))
    val expected = Set(
      Clause(Body(Xx, Y1x, Zx), Head(C1x, C2x, C4x)),
      Clause(Body(Xx, Y2x, Zx), Head(C1x, C3x, C4x))
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }

  test("Hyper-resolution 5") {
    val ontologyClause = OntologyClause(Body(Ax, Bx, Cx), Head())
    val workedOffContextClauses = Map[Predicate, IndexedSequence[ContextClause]](
      Ax -> IndexedSequence(Clause(Body(Xx), Head(Ax, C1x))),
      Bx -> IndexedSequence(Clause(Body(), Head(C2x, Bx)), Clause(Body(Y2x), Head(Bx, C3x)))
    )
    val max = Cx
    val unprocessedClause = Clause(Body(), Head(C4x, Cx))
    val expected = Set(
      Clause(Body(Xx), Head(C1x, C2x, C4x)),
      Clause(Body(Xx, Y2x), Head(C1x, C3x, C4x))
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }

  test("Hyper-resolution 6") {
    val ontologyClause = OntologyClause(Body(Ax, Rxz1), Head(Cz))
    val workedOffContextClauses = Map[Predicate, IndexedSequence[ContextClause]](
      Rxz1 -> IndexedSequence(Clause(Body(Yx), Head(Rxf1)))
    )
    val max = Ax
    val unprocessedClause = Clause(Body(Xx), Head(Ax))
    val expected = Set(
      Clause(Body(Xx, Yx), Head(Cf))
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }

  test("Hyper-resolution 7") {
    val ontologyClause = OntologyClause(Body(Rxz1), Head(Sxz))
    val workedOffContextClauses = Map.empty[Predicate, IndexedSequence[ContextClause]]
    val max = Rxy
    val unprocessedClause = Clause(Body(Rxy), Head(Rxy))
    val expected = Set(
      Clause(Body(Rxy), Head(Sxy))
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }

  test("Hyper-resolution 8") {
    val ontologyClause = OntologyClause(Body(Rxz1, Rxz2), Head())
    val workedOffContextClauses = Map[Predicate, IndexedSequence[ContextClause]](
      Rxz2 -> IndexedSequence(Clause(Body(), Head(Rxf1)))
    )
    val max = Rxz1
    val unprocessedClause = Clause(Body(), Head(Rxf2))
    val expected = Set(
      Clause(Body(), Head())
    )
    val resultsBuffer = new UnprocessedDeque()
    Rules.Hyper(max, unprocessedClause, workedOffContextClauses, ontologyClause, RootContextLiteralOrdering, cutting, isNothing, resultsBuffer)
    val actual = resultsBuffer.iterator.toSet
    assert(decorate(actual) === decorate(expected))
  }

}
