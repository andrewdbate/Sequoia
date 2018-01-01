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

package com.sequoiareasoner.owlapi.impl

import java.util.{Collections => jCollections, Set => jSet}

import org.semanticweb.owlapi.model._

/** Implements the OWLObjectComplementOf inference.
  *
  * A custom implementation allows us to avoid the dependency on OWL API bindings.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param operand  the class expression to complement.
  */
class OWLObjectComplementOfImpl(operand: OWLClassExpression) extends OWLObjectComplementOf {
  override def accept(visitor: OWLClassExpressionVisitor): Unit = visitor.visit(this)
  override def accept(visitor: OWLObjectVisitor): Unit = visitor.visit(this)
  override def accept[O](visitor: OWLClassExpressionVisitorEx[O]): O = visitor.visit(this)
  override def accept[O](visitor: OWLObjectVisitorEx[O]): O = visitor.visit(this)
  override def asConjunctSet: jSet[OWLClassExpression] = jCollections.singleton(this)
  override def asDisjunctSet: jSet[OWLClassExpression] = jCollections.singleton(this)
  override def asOWLClass: OWLClass =
    throw new OWLRuntimeException("Not an OWL class. This method should only be called if isAnonymous returns false!")
  override def compareTo(o: OWLObject): Int = {
    // For compatibility with the OWL API v4; we use the same logic for comparing objects.
    val thisTypeIndex: Int = objectTypeIndexProvider.getTypeIndex(this)
    val thatTypeIndex: Int = objectTypeIndexProvider.getTypeIndex(o)
    val diff = thisTypeIndex - thatTypeIndex
    if (diff != 0) return diff
    operand.compareTo(o.asInstanceOf[OWLObjectComplementOf].getOperand)
  }
  override def containsConjunct(ce: OWLClassExpression): Boolean = ce == this
  override def containsEntityInSignature(owlEntity: OWLEntity): Boolean = operand.containsEntityInSignature(owlEntity)
  override def getAnnotationPropertiesInSignature: jSet[OWLAnnotationProperty] = operand.getAnnotationPropertiesInSignature
  override def getAnonymousIndividuals: jSet[OWLAnonymousIndividual] = operand.getAnonymousIndividuals
  override def getClassesInSignature: jSet[OWLClass] = operand.getClassesInSignature
  override def getClassExpressionType: ClassExpressionType = ClassExpressionType.OBJECT_COMPLEMENT_OF
  override def getComplementNNF: OWLClassExpression = operand.getNNF
  override def getDataPropertiesInSignature: jSet[OWLDataProperty] = operand.getDataPropertiesInSignature
  override def getDatatypesInSignature: jSet[OWLDatatype] = operand.getDatatypesInSignature
  override def getIndividualsInSignature: jSet[OWLNamedIndividual] = operand.getIndividualsInSignature
  override def getNestedClassExpressions: jSet[OWLClassExpression] = {
    val ces = operand.getNestedClassExpressions
    ces.add(this)
    ces
  }
  override def getNNF: OWLClassExpression = operand.getComplementNNF
  override def getObjectComplementOf: OWLClassExpression = operand
  override def getObjectPropertiesInSignature: jSet[OWLObjectProperty] = operand.getObjectPropertiesInSignature
  override def getOperand: OWLClassExpression = operand
  override def getSignature: jSet[OWLEntity] = operand.getSignature
  override def isBottomEntity: Boolean = false
  override def isClassExpressionLiteral: Boolean = !operand.isAnonymous
  override def isOWLNothing: Boolean = false
  override def isOWLThing: Boolean = false
  override def isTopEntity: Boolean = false
}
