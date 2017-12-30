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

package com.sequoiareasoner.kernel.structural

import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.owl.iri.IRI

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Class to convert [[STClause]] clauses to [[OntologyClause]] ontology clauses.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param dlOntology
  */
class STClauseToOntologyClauseTransformer(implicit dlOntology: DLOntology) {
  import Term._

  private[this] val intern = new mutable.AnyRefMap[(Set[Predicate], Boolean), IRI]

  private[this] val z1 = z(1)
  private[this] val sigma = new VariableSwapSubstitution(z1)

  private[this] def predicateSubstitution(sigma: Substitution)(l: Predicate): Predicate = l applySubstitution sigma

  /* TODO: update this explanation to match the code.
   * There is a design choice when when generating auxiliary clauses. For example, suppose we are given the clause
   *   R(x, z_1) \wedge A(z_1) \rightarrow B(x)
   * There is a neighbour variable in a concept the body, which is not allowed. We have
   * have two choices. Either clausify the axiom as
   *                R(x,z_1) \rightarrow B(x) \vee T(z_1)  [Fragment returned for the rest of the axiom]
   *      T(z_2) \wedge A(x) \rightarrow \bottom           [Auxiliary for the ce]
   * where T is a fresh concept name, or clausify the axiom as
   *   R(z_1, x) \wedge A(x) \rightarrow R_A(z_1, x)       [Auxiliary for the ce]
   *             R_A(x, z_2) \rightarrow B(x)              [Fragment returned for the rest of the axiom]
   * where R_A is a fresh concept name. We choose to clausify as the latter, since it preserves Horn wherever
   * possible. This choice makes an enormous difference to the performance of the calculus.
   */

  private[this] def getAuxiliary(conjunction: Seq[Predicate], useRole: Boolean, v: Term): (Predicate, OntologyClause) = {
    require(v.isNeighbourVariable)
    // First map the variable v to the variable z1 used for all auxiliary clauses so the lookup works.
    val standardize = new CentralSubstitution
    standardize.add(v, z1)
    val standardizedConjunction = conjunction map predicateSubstitution(standardize)
    val standardizedConjunctionSet = standardizedConjunction.toSet
    intern.get((standardizedConjunctionSet, useRole)) match {
      case Some(auxIri) =>
        val predicate = if (useRole) Role(auxIri, x, v) else Concept(auxIri, x)
        (predicate, null)
      case None =>
        val auxIri: IRI = intern.getOrElseUpdate((standardizedConjunctionSet, useRole), IRI.all)
        val predicate = if (useRole) Role(auxIri, x, v) else Concept(auxIri, x)
        val clause = OntologyClause(standardizedConjunction map predicateSubstitution(sigma), Seq(if (useRole) Role(auxIri, z1, x) else Concept(auxIri, z1)))
        (predicate, clause)
    }
  }

  /** Transforms a clause that results from structural transformation into a collection of clauses that satisfy the
    * additional restrictions of ontology clauses.
    *
    * @param c  the structural transformation clause to convert.
    * @return the ontology clauses that correspond to the specified clause.
    */
  def transform(c: STClause): Seq[OntologyClause] = {
    if (c.body forall {
      case Concept(_, t) => t.isCentralVariable
      case _ => true
    }) return Seq(OntologyClause(c.body, c.head))
    val conceptNeighbourVars: Set[Term] = c.body.collect {
      case Concept(_, v) if v.isNeighbourVariable => v
    }.toSet
    val conjunctionsToHoist: Map[Term, Seq[Predicate]] = c.body.groupBy {
      case Concept(_, v) => v
      case Role(_, `x`, v) => if (conceptNeighbourVars contains v) v else x
      case Role(_, v, `x`) => if (conceptNeighbourVars contains v) v else x
      case _ => throw new AssertionError("Illegal role.")
    }
    @inline def occursInHead(v: Term): Boolean = c.head.exists {
      case Concept(_, t) => t == v
      case Role(_, s, t) => s == v || t == v
      case Equality(s, t) => s == v || t == v
      case Inequality(s, t) => s == v || t == v
    }
    val predicates = new ArrayBuffer[Predicate]
    val clauses = new ArrayBuffer[OntologyClause]
    for ((v, conjunction) <- conjunctionsToHoist if !v.isCentralVariable) {
      val (p, clause) = getAuxiliary(conjunction, occursInHead(v), v)
      predicates += p
      if (clause ne null) clauses += clause
    }
    Seq(OntologyClause((predicates ++ conjunctionsToHoist.getOrElse(x, Nil)), c.head)) ++ clauses
  }

  /** Transforms a collection of clauses that result from structural transformation into a collection of clauses that
    * satisfy the additional restrictions of ontology clauses.
    *
    * @param cs  the collection of structural transformation clause to convert.
    * @return the ontology clauses that correspond to the specified clauses.
    */
  def transform(cs: Iterable[STClause]): Iterable[OntologyClause] = cs flatMap transform

}
