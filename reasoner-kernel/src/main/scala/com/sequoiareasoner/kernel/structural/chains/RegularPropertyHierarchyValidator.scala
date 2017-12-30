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

package com.sequoiareasoner.kernel.structural.chains

import com.sequoiareasoner.kernel.graph.MutableGraph
import com.sequoiareasoner.kernel.owl.PredefinedObjectProperty
import com.sequoiareasoner.kernel.owl.model._

/** Class that implements an OWL ontology validator for the __Restriction on the Property Hierarchy__ as described in
  * [[http://www.w3.org/TR/owl2-syntax/#The_Restrictions_on_the_Axiom_Closure The Restrictions on the Axiom Closure]]
  * in [[http://www.w3.org/TR/owl2-syntax/ The OWL 2 Structural Specification]].
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class RegularPropertyHierarchyValidator {

  private[this] val partialOrder = new MutableGraph[ObjectPropertyExpression]

  private[this] def addToOrder(ope1: ObjectPropertyExpression, ope2: ObjectPropertyExpression): Unit = {
    partialOrder.addEdge(ope1, ope2)
    partialOrder.addEdge(ope1.inverse, ope2.inverse)
  }

  private[this] val owlTopObjectProperty = PredefinedObjectProperty.owlTopObjectProperty

  /** Checks the specified axiom for conformance with the the restrictions on the property hierarchy, as described
    * in [[http://www.w3.org/TR/owl2-syntax/#The_Restrictions_on_the_Axiom_Closure The Restrictions on the Axiom Closure]].
    *
    * @param axiom  the axiom to check.
    * @return `true` iff the specified axiom adheres to the restrictions on the property hierarchy.
    */
  def visit(axiom: Axiom): Unit = axiom match {
    case SubObjectPropertyOfAxiom(_: ObjectPropertyExpression, `owlTopObjectProperty`) =>
      // We ignore axioms of this form. This is a safe extension of what is permitted by the
      // OWL 2 Structural Specification.
    case SubObjectPropertyOfAxiom(ope1: ObjectPropertyExpression, ope2) =>
      // OPE1 → OPE2 holds.
      addToOrder(ope1, ope2)
    case EquivalentObjectPropertiesAxiom(opes) =>
      // OPE1 → OPE2 and OPE2 → OPE1 for each OPE1 and OPE2 in opes.
      for (ope1 <- opes; ope2 <- opes if ope1 != ope2) { addToOrder(ope1, ope2); addToOrder(ope2, ope1) }
    case InverseObjectPropertiesAxiom(ope1, ope2) =>
      // OPE1 → INV(OPE2) and INV(OPE2) → OPE1 hold.
      addToOrder(ope1, ope2.inverse); addToOrder(ope2.inverse, ope1)
    case SymmetricObjectPropertyAxiom(ope) =>
      // OPE → INV(OPE)
      addToOrder(ope, ope.inverse)
    case SubObjectPropertyOfAxiom(ObjectPropertyChain(opes), ope2) =>
      // Adding both OPE1 → OPE2 and INV(OPE1) → INV(OPE2) is required, but the
      // OWL 2 Structural Specification is incorrect is not prescribing this.
      for (ope1 <- opes) { addToOrder(ope1, ope2) }
    case _  => // Nothing to do.
  }

  def visitEnd: Unit = partialOrder.transitivelyClose

  /** Returns `true` if `ope1 ->* ope2`.
    *
    * @param ope1
    * @param ope2
    * @return `true` iff `ope1 ->* ope2`
    */
  private[this] def lessThan(ope1: ObjectPropertyExpression, ope2: ObjectPropertyExpression): Boolean =
    partialOrder.edgeExists(ope1, ope2)

  /**
    * @param opes a sequence of object property expressions
    * @param ope an object property expression
    * @throws NonRegularPropertyHierarchyException if there exists an o \in opes such that ope <= e
    */
  private[this] def assertChainSmaller(opes: Seq[ObjectPropertyExpression], ope: ObjectPropertyExpression): Unit =
    for (e <- opes if lessThan(ope, e))
      throw NonRegularPropertyHierarchyException(e.namedProperty)

  /**
    * @param ope1  the first object property expression
    * @param ope2  the second object property expression
    * @return `true` iff either `ope1 == ope2` or both `ope1 ->* ope2` and `ope2 ->* ope1`
    */
  private[this] def equiv(ope1: ObjectPropertyExpression, ope2: ObjectPropertyExpression): Boolean =
    ope1 == ope2 || (lessThan(ope1, ope2) && lessThan(ope2, ope1))

  /** Checks the specified axiom for conformance with the the restrictions on the property hierarchy, as described
    * in [[http://www.w3.org/TR/owl2-syntax/#The_Restrictions_on_the_Axiom_Closure The Restrictions on the Axiom Closure]].
    *
    * According to the specification, a strict partial order (i.e., an irreflexive and transitive relation) < on the set of
    * all object property expressions exists (called AllOPE) such that the following conditions are fulfilled:
    *
    * $ - `OP1 < OP2` if and only if `INV(OP1) < OP2` for all object properties `OP1` and `OP2` occurring in `AllOPE`.
    * $ - If `OPE1 < OPE2` holds, then `OPE2 ->* OPE1` does not hold;
    * $ - Each axiom of the form `SubObjectPropertyOf(ObjectPropertyChain( OPE1 ... OPEn ) OPE )` with `n => 2` fulfills the
    *     following conditions:
    * $    - `OPE` is equal to `owl:topObjectProperty`, or
    * $    - `n = 2` and `OPE1 = OPE2 = OPE`, or
    * $    - `OPEi < OPE` for each `1 <= i <= n`, or
    * $    - `OPE1 = OPE` and `OPEi < OPE` for each `2 <= i <= n`, or
    * $    - `OPEn = OPE` and `OPEi < OPE` for each `1 <= i <= n-1`.
    *
    * There is a bug in the OWL 2 Structural Specification that we fix here. In addition to the conditions above, we also
    * require that, if `OPE1 < OPE2` hold, then `INV(OPE1) < INV(OPE2)` holds as well. This behaviour is consistent with
    * the OWL API version 4.
    *
    * @param axiom
    * @return `true` iff the `axiom` adheres to the restrictions on the property hierarchy
    */
  def isValid(axiom: Axiom): Unit = axiom match {
    case SubObjectPropertyOfAxiom(ObjectPropertyChain(_), `owlTopObjectProperty`) =>
      // This is legal.

    case SubObjectPropertyOfAxiom(ObjectPropertyChain(Seq(ope1, ope2)), ope) if equiv(ope1, ope) && equiv(ope2, ope) =>
      // This is legal.

    case SubObjectPropertyOfAxiom(ObjectPropertyChain(opes), ope) if opes.head == ope =>
      assertChainSmaller(opes.tail, ope)

    case SubObjectPropertyOfAxiom(ObjectPropertyChain(opes), ope) if opes.last == ope =>
      assertChainSmaller(opes.init, ope)

    case SubObjectPropertyOfAxiom(ObjectPropertyChain(opes), ope) =>
      assertChainSmaller(opes, ope)

    case _  => // These cases are legal.
  }

}
