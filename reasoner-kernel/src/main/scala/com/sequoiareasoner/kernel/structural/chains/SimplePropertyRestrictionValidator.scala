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
import com.sequoiareasoner.kernel.structural.chains
import com.sequoiareasoner.arrayops._

import scala.collection.mutable

/** Class that implements an OWL ontology validator for the __Restriction on Simple Roles__ as described in
  * [[http://www.w3.org/TR/owl2-syntax/#The_Restrictions_on_the_Axiom_Closure The Restrictions on the Axiom Closure]]
  * in [[http://www.w3.org/TR/owl2-syntax/ The OWL 2 Structural Specification]].
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
final class SimplePropertyRestrictionValidator(axioms: Iterable[Axiom]) {

  private[this] val owlTopObjectProperty = PredefinedObjectProperty.owlTopObjectProperty
  private[this] val owlBottomObjectProperty = PredefinedObjectProperty.owlBottomObjectProperty

  private[this] val compositeProperties = mutable.HashSet[ObjectProperty](owlTopObjectProperty, owlBottomObjectProperty)

  private[this] val invertedPartialOrder = new MutableGraph[ObjectProperty]

  private[this] def addToOrder(op1: ObjectProperty, op2: ObjectProperty): Unit =
    invertedPartialOrder.addEdge(op2, op1)

  private[this] def visit(axiom: Axiom): Unit = axiom match {
    case SubObjectPropertyOfAxiom(ope1: ObjectPropertyExpression, ope2) =>
      // OPE1 → OPE2 holds.
      val p1 = ope1.namedProperty; val p2 = ope2.namedProperty
      addToOrder(p1, p2)
    case EquivalentObjectPropertiesAxiom(opes) =>
      // OPE1 → OPE2 and OPE2 → OPE1 for each OPE1 and OPE2 in opes.
      val opsArray = cmap(opes.toArray){ ope => ope.namedProperty }
      cforeach(opsArray) {
        op1 => cforeach(opsArray){ op2 => if (op1 != op2) { addToOrder(op1, op2); addToOrder(op2, op1) } }
      }
    case InverseObjectPropertiesAxiom(ope1, ope2) =>
      // OPE1 → INV(OPE2) and INV(OPE2) → OPE1 hold.
      val p1 = ope1.namedProperty; val p2 = ope2.namedProperty
      addToOrder(p1, p2); addToOrder(p2, p1)
    case SymmetricObjectPropertyAxiom(ope) =>
      // OPE → INV(OPE)
      // We do not add anything because we consider the reflexive-transitive closure.
    case SubObjectPropertyOfAxiom(ObjectPropertyChain(Seq(ope1)), ope2) =>
      // OPE1 → OPE2 holds.
      val p1 = ope1.namedProperty; val p2 = ope2.namedProperty
      addToOrder(p1, p2)
    case SubObjectPropertyOfAxiom(ObjectPropertyChain(chain), ope) if chain.length > 1 =>
      compositeProperties += ope.namedProperty
    case TransitiveObjectPropertyAxiom(ope) =>
      compositeProperties += ope.namedProperty
    case _  => // Nothing to do.
  }

  private[this] def visitEnd: Unit =
    invertedPartialOrder.transitivelyClose

  for (ax <- axioms) visit(ax)
  visitEnd

  /** An object property is simple if, and only if, its inverse is simple.
    * This is a direct consequence of the definitions in
    * [[http://www.w3.org/TR/owl2-syntax/#Property_Hierarchy_and_Simple_Object_Property_Expressions Section 11 of the OWL 2 Structural Specification]].
    *
    * @param ope the object property expression to check
    * @return `true` iff `ope` is simple with respect to `simpleProperties`
    */
  def isSimple(ope: ObjectPropertyExpression): Boolean = {
    val op: ObjectProperty = ope.namedProperty
    !compositeProperties.contains(op) && (invertedPartialOrder.getSuccessors(op) forall {!compositeProperties.contains(_)})
  }

  /** Collects (some) axioms relevant to the cause of the specified object property expression being composite.
    *
    * @param ope  a non-simple object property expression
    * @return axioms relevant to the cause of the specified object property expression being composite.
    */
  private[this] def collectCompositeAxioms(ope: ObjectPropertyExpression): Iterable[Axiom] = {
    // TODO: build a full explanation for why `ope` is composite.
    val op: ObjectProperty = ope.namedProperty
    val possibleComposites: Set[ObjectProperty] = invertedPartialOrder.getSuccessors(op) + op
    def collect(prop: ObjectProperty): Iterable[Axiom] = {
      val propInverse = prop.inverse
      axioms filter {
        case SubObjectPropertyOfAxiom(ObjectPropertyChain(chain), p) =>
          chain.length > 1 && (p == prop || p == propInverse)
        case TransitiveObjectPropertyAxiom(p) =>
          p == prop || p == propInverse
        case _ => false
      }
    }
    possibleComposites flatMap collect
  }

  /** Checks the specified class expression for conformance with the simple object property restrictions, as described in
    * [[http://www.w3.org/TR/owl2-syntax/#The_Restrictions_on_the_Axiom_Closure The Restrictions on the Axiom Closure]],
    * and throws an exception if the conditions are violated.
    *
    * According to the specification, the following types of axioms can only contain simple object properties:
    * $ - `ObjectMinCardinality`
    * $ - `ObjectMaxCardinality`
    * $ - `ObjectExactCardinality`
    * $ - `ObjectHasSelf`
    *
    * @param ce  the class expression to be checked that it adheres to the restrictions on simple property expressions.
    * @throws UseOfNonSimplePropertyException if a non-simple property is used in a position where only a simply property is allowed.
    */
  private[this] def isValid(axiom: Axiom, ce: ClassExpression): Unit = ce match {
    case ObjectMinCardinality(_, ope, ce) =>
      if (!isSimple(ope))
        throw UseOfNonSimplePropertyInCardinalityRestriction(ope.namedProperty, axiom, collectCompositeAxioms(ope))
      isValid(axiom, ce)
    case ObjectMaxCardinality(_, ope, ce) =>
      if (!isSimple(ope))
        throw UseOfNonSimplePropertyInCardinalityRestriction(ope.namedProperty, axiom, collectCompositeAxioms(ope))
      isValid(axiom, ce)
    case ObjectExactCardinality(_, ope, ce) =>
      if (!isSimple(ope))
        throw UseOfNonSimplePropertyInCardinalityRestriction(ope.namedProperty, axiom, collectCompositeAxioms(ope))
      isValid(axiom, ce)
    case ObjectHasSelf(ope) =>
      if (!isSimple(ope))
        throw UseOfNonSimplePropertyInObjectHasSelf(ope.namedProperty, axiom, collectCompositeAxioms(ope))
    case ObjectAllValuesFrom(_, ce)  => isValid(axiom, ce)
    case ObjectSomeValuesFrom(_, ce) => isValid(axiom, ce)
    case ObjectComplementOf(ce)      => isValid(axiom, ce)
    case ObjectIntersectionOf(ces)   => for (ce <- ces) isValid(axiom, ce)
    case ObjectUnionOf(ces)          => for (ce <- ces) isValid(axiom, ce)
    case _:OWLClass | _:DataAllValuesFrom | _:DataExactCardinality | _:DataHasValue | _:DataMaxCardinality |
         _:DataMinCardinality | _:DataSomeValuesFrom | _:ObjectHasValue | _:ObjectOneOf => // Do nothing.
  }

  /** Checks the specified axiom for conformance with the simple object property restrictions, as described in
    * [[https://www.w3.org/TR/owl2-syntax/#The_Restrictions_on_the_Axiom_Closure The Restrictions on the Axiom Closure]],
    * and throws an exception if the conditions are violated.
    *
    * According to the specification, each class expression of the following forms can contain only simple object properties:
    * $ - `FunctionalObjectProperty`
    * $ - `InverseFunctionalObjectProperty`
    * $ - `IrreflexiveObjectProperty`
    * $ - `AsymmetricObjectProperty`
    * $ - `DisjointObjectProperties`
    *
    * @param axiom  the axioms to be checked that it adheres to the restrictions on simple property expressions.
    * @throws UseOfNonSimplePropertyException if a non-simple property is used in a position where only a simply property is allowed.
    */
  def isValid(axiom: Axiom): Unit = axiom match {
    case ax @ AsymmetricObjectPropertyAxiom(ope) =>
      if (!isSimple(ope))
        throw UseOfNonSimplePropertyInAsymmetricObjectPropertyAxiom(ax, collectCompositeAxioms(ope))
    case ax @ DisjointObjectPropertiesAxiom(opes) =>
      for (ope <- opes)
        if (!isSimple(ope))
          throw UseOfNonSimplePropertyInDisjointObjectPropertiesAxiom(ope.namedProperty, ax, collectCompositeAxioms(ope))
    case ax @ FunctionalObjectPropertyAxiom(ope) =>
      if (!isSimple(ope))
        throw UseOfNonSimplePropertyInFunctionalObjectPropertyAxiom(ax, collectCompositeAxioms(ope))
    case ax @ InverseFunctionalObjectPropertyAxiom(ope) =>
      if (!isSimple(ope))
        throw chains.UseOfNonSimplePropertyInInverseFunctionalObjectPropertyAxiom(ax, collectCompositeAxioms(ope))
    case ax @ IrreflexiveObjectPropertyAxiom(ope) =>
      if (!isSimple(ope))
        throw UseOfNonSimplePropertyInIrreflexiveObjectPropertyAxiom(ax, collectCompositeAxioms(ope))
    case DisjointClassesAxiom(ces)        => for (ce <- ces) isValid(axiom, ce)
    case DisjointUnionAxiom(_, ces)       => for (ce <- ces) isValid(axiom, ce)
    case EquivalentClassesAxiom(ces)      => for (ce <- ces) isValid(axiom, ce)
    case ObjectPropertyDomainAxiom(_, ce) => isValid(axiom, ce)
    case ObjectPropertyRangeAxiom(_, ce)  => isValid(axiom, ce)
    case HasKeyAxiom(ce, _, _)            => isValid(axiom, ce)
    case SubClassOfAxiom(sub, sup)        => isValid(axiom, sub); isValid(axiom, sup)
    case ClassAssertionAxiom(ce, _)       => isValid(axiom, ce)
    case DataPropertyDomainAxiom(_, ce)   => isValid(axiom, ce)
    case _:AnnotationAxiom | _:DataPropertyAssertionAxiom | _:DataPropertyRangeAxiom |
         _:DifferentIndividualsAxiom | _:NegativeDataPropertyAssertionAxiom | _:NegativeObjectPropertyAssertionAxiom |
         _:ReflexiveObjectPropertyAxiom | _:SameIndividualAxiom | _:SubDataPropertyOfAxiom | _:TransitiveObjectPropertyAxiom |
         _:DatatypeDefinitionAxiom | _:DeclarationAxiom | _:DisjointDataPropertiesAxiom | _:EquivalentDataPropertiesAxiom |
         _:EquivalentObjectPropertiesAxiom | _:FunctionalDataPropertyAxiom | _:InverseObjectPropertiesAxiom |
         _:ObjectPropertyAssertionAxiom | _:SubObjectPropertyOfAxiom[_] | _:SymmetricObjectPropertyAxiom => // Do nothing.
  }

}
