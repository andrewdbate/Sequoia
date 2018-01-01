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
import org.semanticweb.owlapi.util.{HashCode, OWLObjectTypeIndexProvider}

/** Implements the OWLClass inference.
  *
  * A custom implementation allows us to avoid the dependency on OWL API bindings.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param iri  the IRI of this class.
  */
final class OWLClassImpl(iri: IRI) extends OWLClass {
  override def accept(visitor: OWLClassExpressionVisitor): Unit = visitor.visit(this)
  override def accept(visitor: OWLEntityVisitor): Unit = visitor.visit(this)
  override def accept(visitor: OWLNamedObjectVisitor): Unit = visitor.visit(this)
  override def accept(visitor: OWLObjectVisitor): Unit = visitor.visit(this)
  override def accept[O](visitor: OWLClassExpressionVisitorEx[O]): O = visitor.visit(this)
  override def accept[O](visitor: OWLEntityVisitorEx[O]): O = visitor.visit(this)
  override def accept[O](visitor: OWLNamedObjectVisitorEx[O]): O = visitor.visit(this)
  override def accept[O](visitor: OWLObjectVisitorEx[O]): O = visitor.visit(this)
  override def asConjunctSet: jSet[OWLClassExpression] = jCollections.singleton(this)
  override def asDisjunctSet: jSet[OWLClassExpression] = jCollections.singleton(this)
  override def asOWLAnnotationProperty: Nothing = throw new OWLRuntimeException("Not an annotation property!")
  override def asOWLClass: this.type = this
  override def asOWLDataProperty: Nothing = throw new OWLRuntimeException("Not a data property!")
  override def asOWLDatatype: Nothing = throw new OWLRuntimeException("Not a data type!")
  override def asOWLNamedIndividual: Nothing = throw new OWLRuntimeException("Not an individual!")
  override def asOWLObjectProperty: Nothing = throw new OWLRuntimeException("Not an object property!")
  override def compareTo(o: OWLObject): Int = {
    // For compatibility with the OWL API v4.
    val thisTypeIndex: Int = OWLObjectTypeIndexProvider.OWL_CLASS
    val thatTypeIndex: Int = objectTypeIndexProvider.getTypeIndex(o)
    val diff = thisTypeIndex - thatTypeIndex
    if (diff != 0) return diff
    iri.compareTo(o.asInstanceOf[OWLClass].getIRI)
  }
  override def containsConjunct(ce: OWLClassExpression): Boolean = ce == this
  override def containsEntityInSignature(owlEntity: OWLEntity): Boolean = owlEntity == this
  override def getAnnotationPropertiesInSignature: jSet[OWLAnnotationProperty] = jCollections.emptySet()
  override def getAnonymousIndividuals: jSet[OWLAnonymousIndividual] = jCollections.emptySet()
  override def getClassesInSignature: jSet[OWLClass] = jCollections.singleton(this)
  override def getClassExpressionType: ClassExpressionType = ClassExpressionType.OWL_CLASS
  override def getComplementNNF: OWLClassExpression = getObjectComplementOf
  override def getDataPropertiesInSignature: jSet[OWLDataProperty] = jCollections.emptySet()
  override def getDatatypesInSignature: jSet[OWLDatatype] = jCollections.emptySet()
  override def getEntityType: EntityType[_] = EntityType.CLASS
  override def getIndividualsInSignature: jSet[OWLNamedIndividual] = jCollections.emptySet()
  override def getIRI: IRI = iri
  override def getNestedClassExpressions: jSet[OWLClassExpression] = jCollections.singleton(this)
  override def getNNF: this.type = this
  override def getObjectComplementOf: OWLClassExpression = new OWLObjectComplementOfImpl(this)
  override def getObjectPropertiesInSignature: jSet[OWLObjectProperty] = jCollections.emptySet()
  override def getSignature: jSet[OWLEntity] = jCollections.singleton(this)
  override def isAnonymous: Boolean = false
  override def isBottomEntity: Boolean = isOWLNothing
  override def isBuiltIn: Boolean = isOWLThing || isOWLNothing
  override def isClassExpressionLiteral: Boolean = true
  override def isOWLAnnotationProperty: Boolean = false
  override def isOWLClass: Boolean = true
  override def isOWLDataProperty: Boolean = false
  override def isOWLDatatype: Boolean = false
  override def isOWLNamedIndividual: Boolean = false
  override def isOWLNothing: Boolean = iri.isNothing
  override def isOWLObjectProperty: Boolean = false
  override def isOWLThing: Boolean = iri.isThing
  override def isTopEntity: Boolean = isOWLThing
  override def isType(entityType: EntityType[_]): Boolean = getEntityType == entityType
  override def toStringID: String = iri.toString
  override def toString: String = iri.toString
  override def equals(o: Any): Boolean = o match {
    case that: OWLClass => this.iri == that.getIRI
    case _ => false
  }
  override def hashCode: Int = HashCode.hashCode(this) // For compatibility with the OWL API.
}
