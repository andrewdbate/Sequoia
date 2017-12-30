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

package com.sequoiareasoner.owlapi.wrapper

import org.semanticweb.owlapi.model._
import com.sequoiareasoner.kernel.owl.iri.{IRI => SequoiaIRI}
import com.sequoiareasoner.kernel.owl.model.{OWLClass => SequoiaOWLClass, _}
import com.sequoiareasoner.owlapi.util.CollectionUtils._
import com.sequoiareasoner.owlapi.wrapper.visitors._

/** Converter from an instance of an OWL API class to the corresponding instance of the Sequoia OWL model class.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object OWLConverter {

  protected[owlapi] def convert(iri: IRI): SequoiaIRI = SequoiaIRI(iri.toString)

  private[this] def convert(nodeId: NodeID): String = nodeId.toString
  
  def convert(OP: OWLObjectProperty): ObjectProperty = ObjectProperty(convert(OP.getIRI))

  protected[wrapper] def convert(OPE: OWLObjectPropertyExpression): ObjectPropertyExpression =
    OPE.accept(OWLObjectPropertyExpressionConverterVisitor)

  def convert(CE: OWLClassExpression): ClassExpression =
    CE.accept(OWLCorrespondingObjectConverterVisitor)

  protected[wrapper] def convert(ind: OWLIndividual): Individual =
    ind.accept(OWLCorrespondingObjectConverterVisitor)

  protected[wrapper] def convert(dpe: OWLDataPropertyExpression): DataPropertyExpression =
    convert(dpe.asOWLDataProperty)

  protected[wrapper] def convert(dr: OWLDataRange): DataRange =
    dr.accept(OWLCorrespondingObjectConverterVisitor)

  protected[wrapper] def convert(dt: OWLDatatype): Datatype = Datatype(convert(dt.getIRI))

  protected[wrapper] def convert(lit: OWLLiteral): Literal =
    Literal(lit.getLiteral, convert(lit.getDatatype))

  protected[wrapper] def convert(entity: OWLEntity): Entity = entity.accept(OWLCorrespondingObjectConverterVisitor)

  def convert(owlClass: OWLClass): SequoiaOWLClass = SequoiaOWLClass(convert(owlClass.getIRI))

  protected[wrapper] def convert(ap: OWLAnnotationProperty): AnnotationProperty =
    AnnotationProperty(convert(ap.getIRI))

  protected[wrapper] def convert(subject: OWLAnnotationSubject): AnnotationSubject =
    OWLAnnotationSubjectValueVisitor.visit(subject)
    
  protected[wrapper] def convert(value: OWLAnnotationValue): AnnotationValue =
    OWLAnnotationSubjectValueVisitor.visit(value)
    
  def convert(owlAnonymousIndividual: OWLAnonymousIndividual): AnonymousIndividual =
    AnonymousIndividual(convert(owlAnonymousIndividual.getID))

  def convert(davf: OWLDataAllValuesFrom): DataAllValuesFrom =
    DataAllValuesFrom(List(convert(davf.getProperty)), convert(davf.getFiller))

  def convert(dco: OWLDataComplementOf): DataComplementOf =
    DataComplementOf(convert(dco.getDataRange))

  def convert(dhv: OWLDataHasValue): DataHasValue =
    DataHasValue(convert(dhv.getProperty), convert(dhv.getFiller))

  def convert(dio: OWLDataIntersectionOf): DataIntersectionOf =
    DataIntersectionOf(buildSet(dio.getOperands)(convert))

  def convert(dec: OWLDataExactCardinality): DataExactCardinality =
    if (dec.isQualified)
      DataExactCardinality(dec.getCardinality, convert(dec.getProperty), convert(dec.getFiller))
    else
      DataExactCardinality(dec.getCardinality, convert(dec.getProperty))

  def convert(dmc: OWLDataMaxCardinality): DataMaxCardinality =
    if (dmc.isQualified)
      DataMaxCardinality(dmc.getCardinality, convert(dmc.getProperty), convert(dmc.getFiller))
    else
      DataMaxCardinality(dmc.getCardinality, convert(dmc.getProperty))

  def convert(dmc: OWLDataMinCardinality): DataMinCardinality =
    if (dmc.isQualified)
      DataMinCardinality(dmc.getCardinality, convert(dmc.getProperty), convert(dmc.getFiller))
    else
      DataMinCardinality(dmc.getCardinality, convert(dmc.getProperty))

  def convert(doo: OWLDataOneOf): DataOneOf =
    DataOneOf(buildSet(doo.getValues)(convert))

  def convert(dp: OWLDataProperty): DataProperty = DataProperty(convert(dp.getIRI))

  def convert(dsvf: OWLDataSomeValuesFrom): DataSomeValuesFrom =
    DataSomeValuesFrom(List(convert(dsvf.getProperty)), convert(dsvf.getFiller))

  def convert(dr: OWLDatatypeRestriction): DatatypeRestriction =
    DatatypeRestriction(convert(dr.getDatatype), buildSet(dr.getFacetRestrictions)(convert).toList)

  def convert(dd: OWLDatatypeDefinitionAxiom): DatatypeDefinitionAxiom =
    DatatypeDefinitionAxiom(convert(dd.getDatatype), convert(dd.getDataRange))

  def convert(duo: OWLDataUnionOf): DataUnionOf =
    DataUnionOf(buildSet(duo.getOperands)(convert))

  def convert(fr: OWLFacetRestriction): FacetRestriction =
    FacetRestriction(convert(fr.getFacet.getIRI), convert(fr.getFacetValue))

  def convert(owlNamedIndividual: OWLNamedIndividual): NamedIndividual =
    NamedIndividual(convert(owlNamedIndividual.getIRI))

  def convert(oavf: OWLObjectAllValuesFrom): ObjectAllValuesFrom =
    ObjectAllValuesFrom(convert(oavf.getProperty), convert(oavf.getFiller))

  def convert(oco: OWLObjectComplementOf): ObjectComplementOf =
    ObjectComplementOf(convert(oco.getOperand))

  def convert(oec: OWLObjectExactCardinality): ObjectExactCardinality =
    if (oec.isQualified)
      ObjectExactCardinality(oec.getCardinality, convert(oec.getProperty), convert(oec.getFiller))
    else
      ObjectExactCardinality(oec.getCardinality, convert(oec.getProperty))

  def convert(ohs: OWLObjectHasSelf): ObjectHasSelf =
    ObjectHasSelf(convert(ohs.getProperty))

  def convert(ohv: OWLObjectHasValue): ObjectHasValue =
    ObjectHasValue(convert(ohv.getProperty), convert(ohv.getFiller))

  def convert(oio: OWLObjectIntersectionOf): ObjectIntersectionOf =
    ObjectIntersectionOf(buildSet(oio.getOperands)(convert))

  def convert(omc: OWLObjectMaxCardinality): ObjectMaxCardinality =
    if (omc.isQualified)
      ObjectMaxCardinality(omc.getCardinality, convert(omc.getProperty), convert(omc.getFiller))
    else
      ObjectMaxCardinality(omc.getCardinality, convert(omc.getProperty))

  def convert(pmc: OWLObjectMinCardinality): ObjectMinCardinality =
    if (pmc.isQualified)
      ObjectMinCardinality(pmc.getCardinality, convert(pmc.getProperty), convert(pmc.getFiller))
    else
      ObjectMinCardinality(pmc.getCardinality, convert(pmc.getProperty))

  def convert(owlObjectOneOf: OWLObjectOneOf): ObjectOneOf =
    ObjectOneOf(buildSet(owlObjectOneOf.getIndividuals)(convert))

  def convert(osvf: OWLObjectSomeValuesFrom): ObjectSomeValuesFrom =
    ObjectSomeValuesFrom(convert(osvf.getProperty), convert(osvf.getFiller))

  def convert(ouo: OWLObjectUnionOf): ObjectUnionOf =
    ObjectUnionOf(buildSet(ouo.getOperands)(convert))

  def convert(axiom: OWLObjectPropertyAssertionAxiom): ObjectPropertyAssertionAxiom =
    ObjectPropertyAssertionAxiom(convert(axiom.getProperty), convert(axiom.getSubject), convert(axiom.getObject))

  def convert(axiom: OWLSubPropertyChainOfAxiom): ObjectPropertyChain =
    ObjectPropertyChain(buildList(axiom.getPropertyChain)(convert))

  def convert(axiom: OWLObjectPropertyDomainAxiom): ObjectPropertyDomainAxiom =
    ObjectPropertyDomainAxiom(convert(axiom.getProperty), convert(axiom.getDomain))

  def convert(axiom: OWLObjectPropertyRangeAxiom): ObjectPropertyRangeAxiom =
    ObjectPropertyRangeAxiom(convert(axiom.getProperty), convert(axiom.getRange))

  def convert(axiom: OWLReflexiveObjectPropertyAxiom): ReflexiveObjectPropertyAxiom =
    ReflexiveObjectPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLSameIndividualAxiom): SameIndividualAxiom =
    SameIndividualAxiom(buildSet(axiom.getIndividuals)(convert))

  def convert(axiom: OWLSubClassOfAxiom): SubClassOfAxiom =
    SubClassOfAxiom(convert(axiom.getSubClass), convert(axiom.getSuperClass))

  def convert(axiom: OWLSubDataPropertyOfAxiom): SubDataPropertyOfAxiom =
    SubDataPropertyOfAxiom(convert(axiom.getSubProperty), convert(axiom.getSuperProperty))

  def convert(axiom: OWLSubObjectPropertyOfAxiom): SubObjectPropertyOfAxiom[SubObjectPropertyExpression] =
    SubObjectPropertyOfAxiom(convert(axiom.getSubProperty), convert(axiom.getSuperProperty))

  def convert(axiom: OWLSymmetricObjectPropertyAxiom): SymmetricObjectPropertyAxiom =
    SymmetricObjectPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLTransitiveObjectPropertyAxiom): TransitiveObjectPropertyAxiom =
    TransitiveObjectPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLHasKeyAxiom): HasKeyAxiom = {
    val objectPropertyExpressions = buildSet(axiom.getObjectPropertyExpressions)(convert)
    val dataPropertyExpressions = buildSet(axiom.getDataPropertyExpressions)(convert)
    HasKeyAxiom(convert(axiom.getClassExpression), objectPropertyExpressions, dataPropertyExpressions)
  }

  def convert(axiom: OWLAsymmetricObjectPropertyAxiom): AsymmetricObjectPropertyAxiom =
    AsymmetricObjectPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLClassAssertionAxiom): ClassAssertionAxiom =
    ClassAssertionAxiom(convert(axiom.getClassExpression), convert(axiom.getIndividual))

  def convert(axiom: OWLAnnotationAxiom): AnnotationAxiom =
    axiom.accept(OWLAnnotationAxiomConverterVisitor)
    
  def convert(axiom: OWLIndividualAxiom): AssertionAxiom =
    axiom.accept(OWLIndividualAxiomConverterVisitor)
    
  def convert(axiom: OWLAxiom): Axiom =
    axiom.accept(OWLAxiomConverterVisitor)
    
  def convert(axiom: OWLClassAxiom): ClassAxiom =
    axiom.accept(OWLClassAxiomConverterVisitor)
    
  def convert(axiom: OWLDataPropertyAxiom): DataPropertyAxiom =
    axiom.accept(OWLDataPropertyAxiomConverterVisitor)
    
  def convert(axiom: OWLObjectPropertyAxiom): ObjectPropertyAxiom =
    axiom.accept(OWLObjectPropertyAxiomConverterVisitor)

  def convert(axiom: OWLDeclarationAxiom): DeclarationAxiom =
    DeclarationAxiom(convert(axiom.getEntity))

  def convert(axiom: OWLDifferentIndividualsAxiom): DifferentIndividualsAxiom =
    DifferentIndividualsAxiom(buildList(axiom.getIndividualsAsList)(convert))

  def convert(axiom: OWLDisjointClassesAxiom): DisjointClassesAxiom =
    DisjointClassesAxiom(buildList(axiom.getClassExpressionsAsList)(convert))

  def convert(axiom: OWLDisjointDataPropertiesAxiom): DisjointDataPropertiesAxiom =
    DisjointDataPropertiesAxiom(buildSet(axiom.getProperties)(convert).toList)

  def convert(axiom: OWLDisjointObjectPropertiesAxiom): DisjointObjectPropertiesAxiom =
    DisjointObjectPropertiesAxiom(buildSet(axiom.getProperties)(convert).toList)

  def convert(axiom: OWLDisjointUnionAxiom): DisjointUnionAxiom =
    DisjointUnionAxiom(convert(axiom.getOWLClass), buildSet(axiom.getClassExpressions)(convert).toList)

  def convert(axiom: OWLEquivalentClassesAxiom): EquivalentClassesAxiom =
    EquivalentClassesAxiom(buildSet(axiom.getClassExpressions)(convert))

  def convert(axiom: OWLEquivalentDataPropertiesAxiom): EquivalentDataPropertiesAxiom =
    EquivalentDataPropertiesAxiom(buildSet(axiom.getProperties)(convert))

  def convert(axiom: OWLEquivalentObjectPropertiesAxiom): EquivalentObjectPropertiesAxiom =
    EquivalentObjectPropertiesAxiom(buildSet(axiom.getProperties)(convert))

  def convert(axiom: OWLFunctionalDataPropertyAxiom): FunctionalDataPropertyAxiom =
    FunctionalDataPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLFunctionalObjectPropertyAxiom): FunctionalObjectPropertyAxiom =
    FunctionalObjectPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLInverseFunctionalObjectPropertyAxiom): InverseFunctionalObjectPropertyAxiom =
    InverseFunctionalObjectPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLInverseObjectPropertiesAxiom): InverseObjectPropertiesAxiom =
    InverseObjectPropertiesAxiom(convert(axiom.getFirstProperty), convert(axiom.getSecondProperty))

  def convert(axiom: OWLIrreflexiveObjectPropertyAxiom): IrreflexiveObjectPropertyAxiom =
    IrreflexiveObjectPropertyAxiom(convert(axiom.getProperty))

  def convert(axiom: OWLDataPropertyAssertionAxiom): DataPropertyAssertionAxiom =
    DataPropertyAssertionAxiom(convert(axiom.getProperty), convert(axiom.getSubject), convert(axiom.getObject))

  def convert(axiom: OWLDataPropertyDomainAxiom): DataPropertyDomainAxiom =
    DataPropertyDomainAxiom(convert(axiom.getProperty), convert(axiom.getDomain))

  def convert(axiom: OWLDataPropertyRangeAxiom): DataPropertyRangeAxiom =
    DataPropertyRangeAxiom(convert(axiom.getProperty), convert(axiom.getRange))

  def convert(axiom: OWLNegativeDataPropertyAssertionAxiom): NegativeDataPropertyAssertionAxiom =
    NegativeDataPropertyAssertionAxiom(convert(axiom.getProperty), convert(axiom.getSubject), convert(axiom.getObject))

  def convert(axiom: OWLNegativeObjectPropertyAssertionAxiom): NegativeObjectPropertyAssertionAxiom =
    NegativeObjectPropertyAssertionAxiom(convert(axiom.getProperty), convert(axiom.getSubject), convert(axiom.getObject))

  /**
    * @param axiom  the OWL axiom to test
    * @return `true` if the owl axiom can be converted to a Sequoia axiom
    */
  def isRelevantAxiom(axiom: OWLAxiom): Boolean = axiom.isLogicalAxiom || axiom.isOfType(AxiomType.DECLARATION)

}
