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

package com.sequoiareasoner.kernel.reasoner

import com.sequoiareasoner.kernel.CommonNames
import com.sequoiareasoner.kernel.taxonomy.{ImmutableIRISet, Taxonomy, TaxonomyNode}
import io.cso._
import com.sequoiareasoner.kernel.owl.iri.IRI

import org.scalatest.FunSuite

/** This file contains some basic tests that the rules of the calculus have been implemented correctly.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class BasicAlgorithmTest extends FunSuite {
  import CommonNames._

  // TODO: this file needs to be updated to account for the changes in the reasoner API.
  /*
  private[this] def base(conceptToClassify: Concept,
                         input: Set[OntologyClause],
                         expected: Set[(Set[IRI], Set[IRI])]): Unit = {
    val toTaxonomy = new LinkedBufferedChannel[(IRI, IRI)](100)
    val fromTaxonomy = new CellBufferedChannel[Taxonomy]

    DLOntology.indexClauses(input)

    fork(proc {
      par(strategy(Set(conceptToClassify), toTaxonomy),
        taxonomy(toTaxonomy, fromTaxonomy))
    })

    val t: Taxonomy = block { fromTaxonomy? }

    val nodes: Set[TaxonomyNode] = t.allNodes.toSet


    val directSuperClasses: Set[(ImmutableIRISet, ImmutableIRISet)] =
      for (n <- nodes; sup <- n.directSuperNodes) yield (n.equivalentClasses, sup.equivalentClasses)

    assert(directSuperClasses === expected)
  }

  test ("Hyper rule only") {
    val input = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(C, x))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
    )
    val conceptToClassify = Concept(A, x)

    val nodeA: Set[IRI] = Set(A.iri)
    val nodeB: Set[IRI] = Set(B.iri)
    val nodeC: Set[IRI] = Set(C.iri)
    val nodeD: Set[IRI] = Set(D.iri)

    val expected = Set((nodeA, nodeB), (nodeA, nodeC), (nodeA, nodeD))

    base(conceptToClassify, input, expected)
  }

  test ("Hyper, Eq, Ineq") {
    val input = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x), Equality(f(2), f(1)))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x), Inequality(f(2), f(1))))
    )
    val conceptToClassify = Concept(A, x)

    val nodeA: Set[IRI] = Set(A.iri)
    val nodeB: Set[IRI] = Set(B.iri)

    val expected = Set((nodeA, nodeB))

    base(conceptToClassify, input, expected)
  }

  test ("Hyper, Succ, Pred") {
    val input = Set(
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x), Concept(C, f(1)))),
      OntologyClause(Body(Concept(A, x)), Head(Concept(B, x), Concept(D, f(1)))),
      OntologyClause(Body(Concept(C, x), Concept(D, x)), Head())
    )
    val conceptToClassify = Concept(A, x)

    val nodeA: Set[IRI] = Set(A.iri)
    val nodeB: Set[IRI] = Set(B.iri)

    val expected = Set((nodeA, nodeB))

    base(conceptToClassify, input, expected)
  }

  test("Example 1 from Simancik and Bate Technical Report (2012)") {
    val input = Set(
      OntologyClause(Body(Concept(A, x)), Head(Role(R, x, f1))),
      OntologyClause(Body(Role(R, x, z1)), Head(Concept(B, x), Concept(C, z1))),
      OntologyClause(Body(Concept(C, x)), Head(Concept(D, x))),
      OntologyClause(Body(Concept(D, x), Role(R, z1, x)), Head(Role(S, x, z1), Concept(B, z1))),
      OntologyClause(Body(Role(R, x, z1), Role(S, z1, x)), Head())
    )
    val conceptToClassify = Concept(A, x)

    val nodeA: Set[IRI] = Set(A.iri)
    val nodeB: Set[IRI] = Set(B.iri)

    val expected = Set((nodeA, nodeB))

    base(conceptToClassify, input, expected)
  }
*/
}
