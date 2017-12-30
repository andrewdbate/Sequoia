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

package com.sequoiareasoner.kernel.owl.printers

import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.owl.model._

/**
 * Prints Sequoia OWL model objects in OWL 2 functional-style syntax.
 */
object OWLFunctionalStylePrinter {
  // TODO: implement tests for this printer.

  /**
   * @precondition owlObjects.nonEmpty
   */
  private[this] def fssStrings(owlObjects: Iterable[OWLObject])(implicit expandAbbreviatedIris: Boolean): String =
    owlObjects map { fssString(_)(expandAbbreviatedIris) } reduce { (x, y) => s"$x $y" }

  /**
   * Prints Sequoia OWL model objects in OWL 2 functional-style syntax, as according to http://www.w3.org/TR/owl2-syntax/.
   *
   * @param owlObject              the Sequoia OWL model object to print
   * @param expandAbbreviatedIris  if true, the printer will print abbreviated IRIs as full IRIs (in "<>")
   */
  def fssString(owlObject: OWLObject)(implicit expandAbbreviatedIris: Boolean = false): String = (owlObject: @unchecked) match {
    case AnnotationProperty(iri)                              => fssString(iri)
    case AnonymousIndividual(nodeId)                          => nodeId
    case AsymmetricObjectPropertyAxiom(property)              => s"AsymmetricObjectProperty(${fssString(property)} })"
    case OWLClass(iri)                                        => fssString(iri)
    case ClassAssertionAxiom(ce, individual)                  => s"ClassAssertion(${fssString(ce)} ${fssString(individual)})"
    case DataAllValuesFrom(dpes, dataRange)                   => s"DataAllValuesFrom(${fssStrings(dpes)} ${fssString(dataRange)})"
    case DataComplementOf(dataRange)                          => s"DataComplementOf(${fssString(dataRange)})"
    case DataExactCardinality(n, dpe, dr) if dr.isTopDatatype => s"DataExactCardinality($n ${fssString(dpe)})"
    case DataExactCardinality(n, dpe, dr)                     => s"DataExactCardinality($n ${fssString(dpe)} ${fssString(dr)})"
    case DataHasValue(dpe, literal)                           => s"DataHasValue(${fssString(dpe)} ${fssString(literal)})"
    case DataIntersectionOf(drs)                              => s"DataIntersectionOf(${fssStrings(drs)})"
    case DataMaxCardinality(n, dpe, dr) if dr.isTopDatatype   => s"DataMaxCardinality($n ${fssString(dpe)})"
    case DataMaxCardinality(n, dpe, dr)                       => s"DataMaxCardinality($n ${fssString(dpe)} ${fssString(dr)})"
    case DataMinCardinality(n, dpe, dr) if dr.isTopDatatype   => s"DataMinCardinality($n ${fssString(dpe)})"
    case DataMinCardinality(n, dpe, dr)                       => s"DataMinCardinality($n ${fssString(dpe)} ${fssString(dr)})"
    case DataOneOf(literals)                                  => s"DataOneOf(${fssStrings(literals)})"
    case DataProperty(iri)                                    => fssString(iri)
    case DataPropertyAssertionAxiom(prop, sub, obj)           => s"DataPropertyAssertion(${fssString(prop)} ${fssString(sub)} ${fssString(obj)})"
    case DataPropertyDomainAxiom(dpe, domain)                 => s"DataPropertyDomain(${fssString(dpe)} ${fssString(domain)})"
    case DataPropertyRangeAxiom(dpe, range)                   => s"DataPropertyRange(${fssString(dpe)} ${fssString(range)})"
    case DataSomeValuesFrom(dpe, dr)                          => s"DataSomeValuesFrom(${fssStrings(dpe)} ${fssString(dr)})"
    case Datatype(iri)                                        => fssString(iri)
    case DatatypeRestriction(datatype, facets)                => s"DatatypeRestriction(${fssString(datatype)} ${fssStrings(facets)})"
    case DataUnionOf(dataRanges)                              => s"DataUnionOf(${fssStrings(dataRanges)})"
    case DeclarationAxiom(entity)                             => s"Declaration(${functionalDeclarationPrinter(entity)})"
    case DifferentIndividualsAxiom(individuals)               => s"DifferentIndividuals(${fssStrings(individuals)})"
    case DisjointClassesAxiom(ces)                            => s"DisjointClasses(${fssStrings(ces)})"
    case DisjointDataPropertiesAxiom(dpe)                     => s"DisjointDataProperties(${fssStrings(dpe)})"
    case DisjointObjectPropertiesAxiom(opes)                  => s"DisjointObjectProperties(${fssStrings(opes)})"
    case DisjointUnionAxiom(c, ces)                           => s"DisjointUnion(${fssString(c)} ${fssStrings(ces)})"
    case EquivalentClassesAxiom(ces)                          => s"EquivalentClasses(${fssStrings(ces)})"
    case EquivalentDataPropertiesAxiom(dpes)                  => s"EquivalentDataProperties(${fssStrings(dpes)})"
    case EquivalentObjectPropertiesAxiom(opes)                => s"DisjointObjectProperties(${fssStrings(opes)})"
    case FacetRestriction(facet, literal)                     => s"${fssString(facet)} ${fssString(literal)}"
    case FunctionalDataPropertyAxiom(dpe)                     => s"FunctionalDataProperty(${fssString(dpe)})"
    case FunctionalObjectPropertyAxiom(ope)                   => s"FunctionalObjectProperty(${fssString(ope)})"
    case InverseFunctionalObjectPropertyAxiom(ope)            => s"InverseFunctionalObjectProperty(${fssString(ope)})"
    case InverseObjectPropertiesAxiom(ope1, ope2)             => s"InverseObjectProperties(${fssString(ope1)} ${fssString(ope2)})"
    case IrreflexiveObjectPropertyAxiom(ope)                  => s"IrreflexiveObjectProperty(${fssString(ope)})"
    case Literal(lexicalForm, datatype)                       => if (datatype.isPlain) s""""$lexicalForm"""" else s""""$lexicalForm"^^${fssString(datatype)}"""
    case NamedIndividual(iri)                                 => fssString(iri)
    case NegativeDataPropertyAssertionAxiom(p, s, o)          => s"NegativeDataPropertyAssertion(${fssString(p)} ${fssString(s)} ${fssString(o)})"
    case NegativeObjectPropertyAssertionAxiom(p, s, o)        => s"NegativeObjectPropertyAssertion(${fssString(p)} ${fssString(s)} ${fssString(o)})"
    case ObjectAllValuesFrom(ope, ce)                         => s"ObjectAllValuesFrom(${fssString(ope)} ${fssString(ce)})"
    case ObjectComplementOf(ce)                               => s"ObjectComplementOf(${fssString(ce)})"
    case ObjectExactCardinality(n, ope, ce) if ce.isThing     => s"ObjectExactCardinality($n ${fssString(ope)})"
    case ObjectExactCardinality(n, ope, ce)                   => s"ObjectExactCardinality($n ${fssString(ope)} ${fssString(ce)})"
    case ObjectHasSelf(ope)                                   => s"ObjectHasSelf(${fssString(ope)})"
    case ObjectHasValue(ope, individual)                      => s"ObjectHasValue(${fssString(ope)} ${fssString(individual)})"
    case ObjectIntersectionOf(ces)                            => s"ObjectIntersectionOf(${fssStrings(ces)})"
    case ObjectInverseOf(op)                                  => s"ObjectInverseOf(${fssString(op)})"
    case ObjectMaxCardinality(n, ope, ce) if ce.isThing       => s"ObjectMaxCardinality($n ${fssString(ope)})"
    case ObjectMaxCardinality(n, ope, ce)                     => s"ObjectMaxCardinality($n ${fssString(ope)} ${fssString(ce)})"
    case ObjectMinCardinality(n, ope, ce) if ce.isThing       => s"ObjectMinCardinality($n ${fssString(ope)})"
    case ObjectMinCardinality(n, ope, ce)                     => s"ObjectMinCardinality($n ${fssString(ope)} ${fssString(ce)})"
    case ObjectOneOf(individuals)                             => s"ObjectOneOf(${fssStrings(individuals)})"
    case ObjectProperty(iri)                                  => fssString(iri)
    case ObjectPropertyAssertionAxiom(prop, sub, obj)         => s"ObjectPropertyAssertion(${fssString(prop)} ${fssString(sub)} ${fssString(obj)})"
    case ObjectPropertyChain(opes)                            => s"ObjectPropertyChain(${fssStrings(opes)})"
    case ObjectPropertyDomainAxiom(prop, domain)              => s"ObjectPropertyDomain(${fssString(prop)} ${fssString(domain)})"
    case ObjectPropertyRangeAxiom(prop, range)                => s"ObjectPropertyRange(${fssString(prop)} ${fssString(range)})"
    case ObjectSomeValuesFrom(ope, ce)                        => s"ObjectSomeValuesFrom(${fssString(ope)} ${fssString(ce)})"
    case ObjectUnionOf(ces)                                   => s"ObjectUnionOf(${fssStrings(ces)})"
    case ReflexiveObjectPropertyAxiom(prop)                   => s"ReflexiveObjectProperty(${fssString(prop)})"
    case SameIndividualAxiom(individuals)                     => s"SameIndividual(${fssStrings(individuals)})"
    case SubClassOfAxiom(sub, sup)                            => s"SubClassOf(${fssString(sub)} ${fssString(sup)})"
    case SubDataPropertyOfAxiom(sub, sup)                     => s"SubDataPropertyOf(${fssString(sub)} ${fssString(sup)})"
    case SubObjectPropertyOfAxiom(sub, sup)                   => s"SubObjectPropertyOf(${fssString(sub)} ${fssString(sup)})"
    case SymmetricObjectPropertyAxiom(ope)                    => s"SymmetricObjectProperty(${fssString(ope)})"
    case TransitiveObjectPropertyAxiom(ope)                   => s"TransitiveObjectProperty(${fssString(ope)})"
    case HasKeyAxiom(ce, ope, dpe)                            => s"HasKey( ${fssString(ce)} ( ${fssStrings(ope)} )( ${fssStrings(dpe)} ))"
    case DatatypeDefinitionAxiom(datatype, dataRange)         => s"DatatypeDefinition( ${fssString(datatype)} ${fssString(dataRange)})"
    case AnnotationAssertionAxiom(prop, sub, value)           => s"AnnotationAssertion( ${fssString(prop)} ${fssString(sub)} ${fssString(value)})"
    case iri: IRI                                             => if (expandAbbreviatedIris) s"<${iri.fullIriAsString}>" else iri.toString
    case SubAnnotationPropertyOfAxiom(sub, sup)               => s"SubAnnotationPropertyOf(${fssString(sub)} ${fssString(sup)})"
    case AnnotationPropertyDomainAxiom(prop, domain)          => s"AnnotationPropertyDomain(${fssString(prop)} ${fssString(domain)})"
    case AnnotationPropertyRangeAxiom(prop, range)            => s"AnnotationPropertyRange(${fssString(prop)} ${fssString(range)})"
    case Annotation(prop, value)                              => s"Annotation(${fssString(prop)} ${fssString(value)})"
  }

  /**
   * An auxiliary method to print declarations by pattern matching.
   */
  private[this] def functionalDeclarationPrinter(e: Entity)(implicit expandAbbreviatedIris: Boolean): String = e match {
    case AnnotationProperty(iri) => s"AnnotationProperty(${fssString(iri)})"
    case OWLClass(iri)           => s"Class(${fssString(iri)})"
    case DataProperty(iri)       => s"DataProperty(${fssString(iri)})"
    case Datatype(iri)           => s"Datatype(${fssString(iri)})"
    case NamedIndividual(iri)    => s"NamedIndividual(${fssString(iri)})"
    case ObjectProperty(iri)     => s"ObjectProperty(${fssString(iri)})"
  }
}
