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

package com.sequoiareasoner.owlapi.wrapper.visitors

import org.semanticweb.owlapi.model._
import com.sequoiareasoner.kernel.owl.model.Axiom

/** A prototype implementation of the [[org.semanticweb.owlapi.model.OWLAxiomVisitorEx]] interface for conversion of OWL
  * API axioms. All visitor methods throw exceptions by default and hence relevant methods should be overridden by
  * subclasses.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @tparam T  the output of the visitor
  */
abstract class AbstractOWLAxiomConverterVisitor[T <: Axiom](targetClass: Class[T]) extends OWLAxiomVisitorEx[T] {

  private[this] def illegalConversionFrom(t: Class[_]): Nothing =
    throw new IllegalArgumentException(s"${t.getName} cannot be converted to ${targetClass.getName}.")

  override def visit(axiom: OWLSubAnnotationPropertyOfAxiom): T =
    illegalConversionFrom(classOf[OWLSubAnnotationPropertyOfAxiom])

  override def visit(axiom: OWLAnnotationPropertyDomainAxiom): T =
    illegalConversionFrom(classOf[OWLAnnotationPropertyDomainAxiom])

  override def visit(axiom: OWLAnnotationPropertyRangeAxiom): T =
    illegalConversionFrom(classOf[OWLAnnotationPropertyRangeAxiom])

  override def visit(axiom: OWLSubClassOfAxiom): T =
    illegalConversionFrom(classOf[OWLSubClassOfAxiom])

  override def visit(axiom: OWLNegativeObjectPropertyAssertionAxiom): T =
    illegalConversionFrom(classOf[OWLNegativeObjectPropertyAssertionAxiom])

  override def visit(axiom: OWLAsymmetricObjectPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLAsymmetricObjectPropertyAxiom])

  override def visit(axiom: OWLReflexiveObjectPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLReflexiveObjectPropertyAxiom])

  override def visit(axiom: OWLDisjointClassesAxiom): T =
    illegalConversionFrom(classOf[OWLDisjointClassesAxiom])

  override def visit(axiom: OWLDataPropertyDomainAxiom): T =
    illegalConversionFrom(classOf[OWLDataPropertyDomainAxiom])

  override def visit(axiom: OWLObjectPropertyDomainAxiom): T =
    illegalConversionFrom(classOf[OWLObjectPropertyDomainAxiom])

  override def visit(axiom: OWLEquivalentObjectPropertiesAxiom): T =
    illegalConversionFrom(classOf[OWLEquivalentObjectPropertiesAxiom])

  override def visit(axiom: OWLNegativeDataPropertyAssertionAxiom): T =
    illegalConversionFrom(classOf[OWLNegativeDataPropertyAssertionAxiom])

  override def visit(axiom: OWLDifferentIndividualsAxiom): T =
    illegalConversionFrom(classOf[OWLDifferentIndividualsAxiom])

  override def visit(axiom: OWLDisjointDataPropertiesAxiom): T =
    illegalConversionFrom(classOf[OWLDisjointDataPropertiesAxiom])

  override def visit(axiom: OWLDisjointObjectPropertiesAxiom): T =
    illegalConversionFrom(classOf[OWLDisjointObjectPropertiesAxiom])

  override def visit(axiom: OWLObjectPropertyRangeAxiom): T =
    illegalConversionFrom(classOf[OWLObjectPropertyRangeAxiom])

  override def visit(axiom: OWLObjectPropertyAssertionAxiom): T =
    illegalConversionFrom(classOf[OWLObjectPropertyAssertionAxiom])

  override def visit(axiom: OWLFunctionalObjectPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLFunctionalObjectPropertyAxiom])

  override def visit(axiom: OWLSubObjectPropertyOfAxiom): T =
    illegalConversionFrom(classOf[OWLSubObjectPropertyOfAxiom])

  override def visit(axiom: OWLDisjointUnionAxiom): T =
    illegalConversionFrom(classOf[OWLDisjointUnionAxiom])

  override def visit(axiom: OWLDeclarationAxiom): T =
    illegalConversionFrom(classOf[OWLDeclarationAxiom])

  override def visit(axiom: OWLAnnotationAssertionAxiom): T =
    illegalConversionFrom(classOf[OWLAnnotationAssertionAxiom])

  override def visit(axiom: OWLSymmetricObjectPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLSymmetricObjectPropertyAxiom])

  override def visit(axiom: OWLDataPropertyRangeAxiom): T =
    illegalConversionFrom(classOf[OWLDataPropertyRangeAxiom])

  override def visit(axiom: OWLFunctionalDataPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLFunctionalDataPropertyAxiom])

  override def visit(axiom: OWLEquivalentDataPropertiesAxiom): T =
    illegalConversionFrom(classOf[OWLEquivalentDataPropertiesAxiom])

  override def visit(axiom: OWLClassAssertionAxiom): T =
    illegalConversionFrom(classOf[OWLClassAssertionAxiom])

  override def visit(axiom: OWLEquivalentClassesAxiom): T =
    illegalConversionFrom(classOf[OWLEquivalentClassesAxiom])

  override def visit(axiom: OWLDataPropertyAssertionAxiom): T =
    illegalConversionFrom(classOf[OWLDataPropertyAssertionAxiom])

  override def visit(axiom: OWLTransitiveObjectPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLTransitiveObjectPropertyAxiom])

  override def visit(axiom: OWLIrreflexiveObjectPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLIrreflexiveObjectPropertyAxiom])

  override def visit(axiom: OWLSubDataPropertyOfAxiom): T =
    illegalConversionFrom(classOf[OWLSubDataPropertyOfAxiom])

  override def visit(axiom: OWLInverseFunctionalObjectPropertyAxiom): T =
    illegalConversionFrom(classOf[OWLInverseFunctionalObjectPropertyAxiom])

  override def visit(axiom: OWLSameIndividualAxiom): T =
    illegalConversionFrom(classOf[OWLSameIndividualAxiom])

  override def visit(axiom: OWLSubPropertyChainOfAxiom): T =
    illegalConversionFrom(classOf[OWLSubPropertyChainOfAxiom])

  override def visit(axiom: OWLInverseObjectPropertiesAxiom): T =
    illegalConversionFrom(classOf[OWLInverseObjectPropertiesAxiom])

  override def visit(axiom: OWLHasKeyAxiom): T =
    illegalConversionFrom(classOf[OWLHasKeyAxiom])

  override def visit(axiom: OWLDatatypeDefinitionAxiom): T =
    illegalConversionFrom(classOf[OWLDatatypeDefinitionAxiom])

  override def visit(rule: SWRLRule): T =
    illegalConversionFrom(classOf[SWRLRule])

}
