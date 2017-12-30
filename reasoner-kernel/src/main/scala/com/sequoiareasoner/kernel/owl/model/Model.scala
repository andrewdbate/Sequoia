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

package com.sequoiareasoner.kernel.owl.model

import com.sequoiareasoner.kernel.owl.iri.IRI

import com.sequoiareasoner.kernel.owl.PredefinedOWLClass.{owlThing, owlNothing}
import com.sequoiareasoner.kernel.owl.Datatypes.rdfsLiteral

/**
  * OWLObjects are immutable and are typically syntactic structures like axioms or class expressions.
  */
sealed trait OWLObject extends Any

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Axioms axiom]] in the OWL 2 specification.
  */
sealed trait Axiom extends OWLObject

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Data_Ranges Data Range]] in the OWL 2 specification.
  */
sealed trait DataRange extends OWLObject {

  /** Returns `true` if and only if this data range is the top data type.
    *
    * @return `true` if and only if this data range is the top data type
    */
  def isTopDatatype: Boolean

  /** Returns the negation-normal form of this data range.
    *
    * @return the negation-normal form of this data range
    */
  def nnf: DataRange

  /** Returns the negation-normal form of the complement of this data range.
    *
    * @return the negation-normal form of the complement of this data range
    */
  def nnfComplement: DataRange

  /** Returns the simplified version of this data range.
    *
    * @return the simplified version of this data range
    */
  def simplified: DataRange
}

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Class_Expressions Class Expression]] in the OWL 2 specification.
  */
sealed trait ClassExpression extends OWLObject {

  /** Returns `true` if and only if this expression is the built in class `owl:Thing`.
    * This method does not determine if the class is equivalent to `owl:Thing`.
    *
    * @return `true` if and only if this expression is `owl:Thing`
    */
  def isThing: Boolean

  /** Returns `true` if and only if this expression is the built in class `owl:Nothing`.
    * This method does not determine if the class is equivalent to `owl:Nothing`.
    *
    * @return `true` if and only if this expression is `owl:Nothing`
    */
  def isNothing: Boolean

  /** Returns the negation-normal form of this expression.
    *
    * @return the expression in negation-normal form
    */
  def nnf: ClassExpression

  /** Returns the negation-normal form of the complement of this expression.
    *
    * @return the NNF of the complement of this expression
    */
  def nnfComplement: ClassExpression
}

/** Either an IRI, an anonymous individual, or a literal, as defined in
  * [[http://www.w3.org/TR/owl2-syntax/#Annotations Section 10]] of the specification.
  */
trait AnnotationValue extends Any with OWLObject

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Property_Axioms Object Property Axiom]] in the OWL 2 specification.
  */
sealed trait ObjectPropertyAxiom extends Axiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Subproperties sub object property expressions]] in the OWL 2 specification.
  */
sealed trait SubObjectPropertyExpression extends OWLObject

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Property_Expressions Object Property Expression]] in the OWL 2 specification.
  */
sealed trait ObjectPropertyExpression extends SubObjectPropertyExpression {
  def namedProperty: ObjectProperty
  def inverse: ObjectPropertyExpression
  def isTopObjectProperty: Boolean
}

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Assertions Assertions]] in the OWL 2 specification.
  */
sealed trait AssertionAxiom extends Axiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Individuals Individual]] in the OWL 2 specification.
  */
sealed trait Individual extends OWLObject

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Entities.2C_Literals.2C_and_Anonymous_Individuals Entity]] in the OWL 2 specification.
  */
sealed trait Entity extends OWLObject {

  /**
    * @return The IRI of this entity.
    */
  def iri: IRI

  override def equals(obj: Any) = obj match { // FIXME: remove this method
    case that: Entity => this.iri == that.iri
    case _            => false
  }
}

/**
  * Implementation for OWLObjects that maintain an IRI.
  */
sealed abstract class IRIObject(val iri: IRI) extends OWLObject { // TODO: remove this?

  override def hashCode: Int = iri.hashCode

  override def equals(obj: Any): Boolean = obj match {
    case other: IRIObject => iri == other.iri
    case _                => false
  }
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Data_Property_Axioms Data Property Axiom]] in the OWL 2 specification.
  */
sealed trait DataPropertyAxiom extends Axiom

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Class_Expression_Axioms Class Expression Axiom]] in the OWL 2 specification.
  */
sealed trait ClassAxiom extends Axiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Data_Property_Expressions Data Property Expression]] in the OWL 2 specification.
  */
sealed trait DataPropertyExpression extends OWLObject

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Annotation_Axioms Annotation Axiom]] in
  * the OWL 2 specification. Annotation axioms do not have any logical meaning and can be safely ignored during
  * reasoning. A (dummy) implementation of annotation axioms is required to handle them more gracefully during
  * loading of OWL 2 ontologies.
  */
sealed trait AnnotationAxiom extends Axiom

/**
  * Either an IRI or an anonymous individual
  */
trait AnnotationSubject extends Any with OWLObject

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Datatype_Definitions Datatype Definitions]] in the OWL 2 specification.
  */
final case class DatatypeDefinitionAxiom(datatype: Datatype, dataRange: DataRange) extends Axiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Existential_Quantification_2 Existential
  * Quantification Data Property Restriction]] in the OWL 2 specification.
  */
final case class DataSomeValuesFrom(dataPropertyExpressions: List[DataPropertyExpression], dr: DataRange) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement = DataAllValuesFrom(dataPropertyExpressions, dr.nnfComplement)
}

/** The basic interface for annotation objects in OWL 2 as defined in
  * [[http://www.w3.org/TR/owl2-syntax/#Annotations Section 10]] of the specification.
  */
final case class Annotation(property: AnnotationProperty, value: AnnotationValue) extends OWLObject

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Reflexive_Object_Properties Reflexive Object Property Axiom]] in the OWL 2 specification.
  */
final case class ReflexiveObjectPropertyAxiom(property: ObjectPropertyExpression) extends ObjectPropertyAxiom

/** Corresponds to the [[http://www.w3.org/TR/owl2-syntax/#Complement_of_Class_Expressions complement of a class expression]] in the OWL 2 specification.
  *
  * @param ce  the class expression that this expression refers to
  */
final case class ObjectComplementOf(ce: ClassExpression) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = ce.nnfComplement
  override def nnfComplement = ce.nnf
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Universal_Quantification Universal Quantification Object Property Restriction]] in the OWL 2 specification.
  */
final case class ObjectAllValuesFrom(ope: ObjectPropertyExpression, ce: ClassExpression) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = ObjectAllValuesFrom(ope, ce.nnf)
  override def nnfComplement = ObjectSomeValuesFrom(ope, ce.nnfComplement)
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Negative_Object_Property_Assertions negative object property assertion axiom]] in the OWL 2 specification.
  */
final case class NegativeObjectPropertyAssertionAxiom(property: ObjectPropertyExpression, subject: Individual, obj: Individual)
  extends AssertionAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Negative_Data_Property_Assertions negative data property assertion axiom]] in the OWL 2 specification.
  */
final case class NegativeDataPropertyAssertionAxiom(property: DataPropertyExpression, subject: Individual, obj: Literal)
  extends AssertionAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Named_Individuals Named Individuals]] in the OWL 2 specification.
  */
final case class NamedIndividual(override val iri: IRI) extends IRIObject(iri) with Entity with Individual

/** Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Literals Literal]] in the OWL 2 specification.
  *
  * The OWL specification distinguishes three specific kinds of literals: typed literals, plain literals without
  * language tag, and plain literals with language tag. However, plain literals are identified with typed literals of
  * type "rdf:PlainLiteral" where the language tag is represented as part of the lexical form. For example,
  * "Some text"@de is represented as "Some text@de"^^rdf:PlainLiteral and "Another text" is represented as
  * "Another text@"^^rdf:PlainLiteral. These forms are considered structurally identical.
  *
  * Therefore, all literals can be considered as consisting of a lexical form and a datatype.
  *
  * Also note that the semantic interpretation of literals is not part of the structural model of OWL. For example, the
  * literals "1"^^xsd:integer, "+1"^^xsd:integer, and "1"^^xsd:shortint are all different.
  *
  * @param lexicalForm  the lexical form of this literal
  * @param datatype     the datatype of this literal (Note that "untyped" literals use the datatype rdf:PlainLiteral in OWL.)
  */
final case class Literal(lexicalForm: String, datatype: Datatype) extends AnnotationValue

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Irreflexive_Object_Properties Irreflexive Object Property Axiom]] in the OWL 2 specification.
  */
final case class IrreflexiveObjectPropertyAxiom(ope: ObjectPropertyExpression) extends ObjectPropertyAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties_2 Inverse Object Properties Axiom]] in the OWL 2 specification.
  *
  * @param first   the first object property that this axiom refers to.
  * @param second  the second object property that this axiom refers to.
  */
final case class InverseObjectPropertiesAxiom(first: ObjectPropertyExpression, second: ObjectPropertyExpression) extends ObjectPropertyAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Inverse-Functional_Object_Properties Inverse Functional Object Property Axiom]] in the OWL 2 specification.
  */
final case class InverseFunctionalObjectPropertyAxiom(ope: ObjectPropertyExpression) extends ObjectPropertyAxiom

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Functional_Object_Properties Functional Object Property Axiom]] in the OWL 2 specification.
  */
final case class FunctionalObjectPropertyAxiom(ope: ObjectPropertyExpression) extends ObjectPropertyAxiom

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Keys Keys]] in the OWL 2 specification.
  */
final case class HasKeyAxiom(ce: ClassExpression, objectPropertyExpressions: Set[ObjectPropertyExpression], dataPropertyExpressions: Set[DataPropertyExpression])
  extends Axiom

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Functional_Data_Properties Functional Data Property Axiom]] in the OWL 2 specification.
  */
final case class FunctionalDataPropertyAxiom(property: DataPropertyExpression) extends DataPropertyAxiom

/** Corresponds to a pair of constraining facet and restriction value as used in
  * [[http://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions OWL 2 Datatype Restrictions]].
  *
  * @param constrainingFacet  IRI of the constraining facet.
  * @param restrictionValue   the literal used as restriction value
  */
final case class FacetRestriction(constrainingFacet: IRI, restrictionValue: Literal)
  extends OWLObject // This object does not exist in the standard OWL2 syntax.

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Equivalent_Object_Properties Equivalent Object Properties Axiom]] in the OWL 2 specification.
  *
  * @param equivalentObjectPropertyExpressions  set of equivalent object property expressions
  */
final case class EquivalentObjectPropertiesAxiom(equivalentObjectPropertyExpressions: Set[ObjectPropertyExpression])
  extends ObjectPropertyAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Equivalent_Data_Properties Equivalent Data Properties Axiom]] in the OWL 2 specification.
  */
final case class EquivalentDataPropertiesAxiom(equivalentDataPropertyExpressions: Set[DataPropertyExpression])
  extends DataPropertyAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Equivalent_Classes Equivalent Class Axiom]] in the OWL 2 specification.
  *
  * @return equivalentClassExpressions  list of equivalent class expressions
  */
final case class EquivalentClassesAxiom(equivalentClassExpressions: Set[ClassExpression]) extends ClassAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions Disjoint Union of Class Expressions Axiom]] in the OWL 2 specification.
  *
  * TODO: use a multiset instead of a list
  *
  * @param definedClass              the class that is defined to be a disjoint union
  * @param disjointClassExpressions  list of disjoint class expressions
  */
final case class DisjointUnionAxiom(definedClass: OWLClass, disjointClassExpressions: List[ClassExpression]) extends ClassAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Disjoint_Object_Properties Disjoint Object Properties Axiom]] in the OWL 2 specification.
  */
final case class DisjointObjectPropertiesAxiom(disjointObjectPropertyExpressions: List[ObjectPropertyExpression])
  extends ObjectPropertyAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Disjoint_Data_Properties Disjoint Data Properties Axiom]] in the OWL 2 specification.
  */
final case class DisjointDataPropertiesAxiom(disjointDataPropertyExpressions: List[DataPropertyExpression])
  extends DataPropertyAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Disjoint_Classes Disjoint Classes Axiom]] in the OWL 2 specification.
  *
  *  TODO: we should use a multiset instead of a list because order is does not affect the semantics, but repetition does (A disjoint from A => A is nothing)
  *
  * @param disjointClassExpressions  list of disjoint class expressions
  */
final case class DisjointClassesAxiom(disjointClassExpressions: Seq[ClassExpression])
  extends ClassAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Individual_Inequality individual inequality axiom]] in the OWL 2 specification.
  *
  * @param individuals  list of individuals
  */
final case class DifferentIndividualsAxiom(individuals: Seq[Individual]) extends AssertionAxiom

/** Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing Declaration Axiom]] in the OWL 2 specification.
  *
  * @param entity   the entity that this declaration axioms refers to.
  */
final case class DeclarationAxiom(entity: Entity) extends Axiom

/** Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges Union of Data Ranges]] in the OWL 2 specification.
  *
  * @param dataRanges  the list of data ranges that this expression refers to
  */
final case class DataUnionOf(dataRanges: Set[DataRange]) extends DataRange {
  override def isTopDatatype: Boolean = false
  override def nnf = DataUnionOf(dataRanges map {_.nnf})
  override def nnfComplement = DataIntersectionOf(dataRanges map {_.nnfComplement})
  override def simplified: DataRange = ???
}

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions Datatype Restrictions]] in the OWL 2 specification.
  *
  * @param datatype           the datatype of this datatype restriction.
  * @param facetRestrictions  the facet restrictions of this datatype restriction.
  */
final case class DatatypeRestriction(datatype: Datatype, facetRestrictions: List[FacetRestriction]) extends DataRange {
  override def isTopDatatype: Boolean = false
  override def nnf = this
  override def nnfComplement = DataComplementOf(this)
  override def simplified = this
}

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Datatypes Datatype]] in the OWL 2 specification.
  */
final case class Datatype(override val iri: IRI) extends IRIObject(iri) with Entity with DataRange {
  def isPlain: Boolean = iri.isPlainLiteral
  override def isTopDatatype: Boolean = iri.isRDFSLiteral
  override def nnf = this
  override def nnfComplement = DataComplementOf(this)
  override def simplified = this
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Data_Property_Range Data Property Range]] in the OWL 2 specification.
  */
final case class DataPropertyRangeAxiom(property: DataPropertyExpression, range: DataRange) extends DataPropertyAxiom

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Transitive_Object_Properties Transitive Object Property Axiom]] in the OWL 2 specification.
  */
final case class TransitiveObjectPropertyAxiom(objectPropertyExpression: ObjectPropertyExpression) extends ObjectPropertyAxiom

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Symmetric_Object_Properties Symmetric Object Property Axiom]] in the OWL 2 specification.
  */
final case class SymmetricObjectPropertyAxiom(property: ObjectPropertyExpression) extends ObjectPropertyAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Subproperties Object Subproperty Axiom]] in the OWL 2 specification.
  *
  * @param subObjectPropertyExpression    sub object property expression of this axiom
  * @param superObjectPropertyExpression  super object property expression of this axiom
  * @tparam E the type of expression on the left-hand side of the axiom (either chain or object property)
  */
final case class SubObjectPropertyOfAxiom[+E <: SubObjectPropertyExpression](subObjectPropertyExpression: E, superObjectPropertyExpression: ObjectPropertyExpression)
  extends Object with ObjectPropertyAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Data_Property_Domain Data Property Domain]] in the OWL 2 specification.
  */
final case class DataPropertyDomainAxiom(dpe: DataPropertyExpression, domain: ClassExpression) extends DataPropertyAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Data_Subproperties Data Subproperty Axiom]] in the OWL 2 specification.
  *
  * @param subDataPropertyExpression    sub data property expression of this axiom
  * @param superDataPropertyExpression  super data property expression of this axiom
  */
final case class SubDataPropertyOfAxiom(subDataPropertyExpression: DataPropertyExpression, superDataPropertyExpression: DataPropertyExpression)
  extends DataPropertyAxiom

/** Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Subclass_Axioms Subclass Axiom]] in the OWL 2 specification.
  *
  * @param subClassExpression    sub class expression of this axiom
  * @param superClassExpression  super class expression of this axiom
  */
final case class SubClassOfAxiom(subClassExpression: ClassExpression, superClassExpression: ClassExpression) extends ClassAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Annotation_Subproperties Annotation Subproperty Axiom]] in the OWL 2 specification.
  *
  * @param subAnnotationProperty    sub annotation property of this axiom.
  * @param superAnnotationProperty  super annotation property of this axiom.
  */
final case class SubAnnotationPropertyOfAxiom(subAnnotationProperty: AnnotationProperty, superAnnotationProperty: AnnotationProperty)
  extends AnnotationAxiom {
  override def toString: String = "SubObjectPropertyOf(" + subAnnotationProperty + " " + superAnnotationProperty + ")"
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Positive_Data_Property_Assertions positive data property assertion axiom]] in the OWL 2 specification.
  */
final case class DataPropertyAssertionAxiom(property: DataPropertyExpression, subject: Individual, obj: Literal)
  extends AssertionAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Data_Properties Data Property]] in the OWL 2 specification.
  */
final case class DataProperty(override val iri: IRI) extends IRIObject(iri) with Entity with DataPropertyExpression

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals Enumeration of Literals]] in the OWL 2 specification.
  *
  * @param literals  the list of literals that this expression refers to
  */
final case class DataOneOf(literals: Set[Literal]) extends DataRange {
  require(literals.nonEmpty)
  override def isTopDatatype: Boolean = false
  override def nnf = this
  override def nnfComplement: DataRange =
    if (literals.size == 1) DataComplementOf(this)
    else DataIntersectionOf(literals map { lit: Literal => DataComplementOf(DataOneOf(Set(lit))) })
  override def simplified = this
}

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Individual_Equality individual equality axiom]] in the OWL 2 specification.
  *
  * @param individuals  the list of individuals that this axiom refers to
  */
final case class SameIndividualAxiom(individuals: Set[Individual]) extends AssertionAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality_2 minimum cardinality restriction]] in the OWL 2 specification.
  */
final case class DataMinCardinality(cardinality: Int, dpe: DataPropertyExpression, dr: DataRange = rdfsLiteral) extends ClassExpression {
  require(cardinality >= 0)
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement: ClassExpression =
    if (cardinality == 0) owlNothing
    else DataMaxCardinality(cardinality - 1, dpe, dr)
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality_2 maximum cardinality restriction]] in the OWL 2 specification.
  */
final case class DataMaxCardinality(cardinality: Int, dpe: DataPropertyExpression, dr: DataRange = rdfsLiteral) extends ClassExpression {
  require(cardinality >= 0)
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement = DataMinCardinality(cardinality + 1, dpe, dr)
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Exact_Cardinality_2 exact cardinality restriction]] in the OWL 2 specification.
  */
final case class DataExactCardinality(cardinality: Int, dpe: DataPropertyExpression, dr: DataRange = rdfsLiteral) extends ClassExpression {
  require(cardinality >= 0)
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement: ClassExpression =
    if (cardinality == 0)
      DataMinCardinality(1, dpe, dr)
    else
      ObjectUnionOf(Set(DataMaxCardinality(cardinality - 1, dpe, dr), DataMinCardinality(cardinality + 1, dpe, dr)))
}

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges Intersection of Data Ranges]] in the OWL 2 specification.
  *
  * @param dataRanges  list of data ranges
  */
final case class DataIntersectionOf(dataRanges: Set[DataRange]) extends DataRange {
  override def isTopDatatype: Boolean = false
  override def nnf = DataIntersectionOf(dataRanges map {_.nnf})
  override def nnfComplement = DataUnionOf(dataRanges map {_.nnf})
  override def simplified: DataRange = ???
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Literal_Value_Restriction Literal Value Restriction for Data Properties]] in the OWL 2 specification.
  */
final case class DataHasValue(dpe: DataPropertyExpression, literal: Literal) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement = DataAllValuesFrom(List(dpe), DataComplementOf(DataOneOf(Set(literal))))
}

/** Corresponds to the [[http://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges complement of a data range]] in the OWL 2 specification.
  *
  * @param dr  data range that this expression refers to
  */
final case class DataComplementOf(dr: DataRange) extends DataRange {
  override def isTopDatatype: Boolean = false
  override def nnf: DataRange = dr.nnfComplement
  override def nnfComplement: DataRange = dr.nnf
  override def simplified: DataRange = dr.simplified match {
    case DataComplementOf(dr) => dr
    case dr => DataComplementOf(dr)
  }
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Universal_Quantification_2 Universal Quantification Data Property Restriction]] in the OWL 2 specification.
  */
final case class DataAllValuesFrom(dataPropertyExpressions: List[DataPropertyExpression], dr: DataRange) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement = DataSomeValuesFrom(dataPropertyExpressions, dr.nnfComplement)
}

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Class_Assertions class assertion axiom]] in the OWL 2 specification.
  *
  * @param ce          the class expression that this axiom refers to.
  * @param individual  the individual that this axiom refers to
  */
final case class ClassAssertionAxiom(ce: ClassExpression, individual: Individual) extends AssertionAxiom

/** Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Union_of_Class_Expressions Union of Class Expressions]] in the OWL 2 specification.
  *
  * @param ces  set of class expressions
  */
final case class ObjectUnionOf(ces: Set[ClassExpression]) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = ObjectUnionOf(ces map {_.nnf})
  override def nnfComplement = ObjectIntersectionOf(ces map {_.nnfComplement})
}

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Classes Class]] in the OWL 2 specification.
  */
final case class OWLClass(override val iri: IRI) extends IRIObject(iri) with Entity with ClassExpression {
  override def isThing: Boolean = iri.isThing
  override def isNothing: Boolean = iri.isNothing
  override def nnf = this
  override def nnfComplement =
    if (isThing) owlNothing
    else if (isNothing) owlThing
    else ObjectComplementOf(this)
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Existential_Quantification Existential Quantification Object Property Restriction]] in the OWL 2 specification.
  */
final case class ObjectSomeValuesFrom(ope: ObjectPropertyExpression, ce: ClassExpression) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = ObjectSomeValuesFrom(ope, ce.nnf)
  override def nnfComplement = ObjectAllValuesFrom(ope, ce.nnfComplement)
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Property_Range Object Property Range]] in the OWL 2 specification.
  */
final case class ObjectPropertyRangeAxiom(ope: ObjectPropertyExpression, range: ClassExpression) extends ObjectPropertyAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Property_Domain Object Property Domain]] in the OWL 2 specification.
  */
final case class ObjectPropertyDomainAxiom(ope: ObjectPropertyExpression, domain: ClassExpression) extends ObjectPropertyAxiom

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Asymmetric_Object_Properties Asymmetric Object Property Axiom]] in the OWL 2 specification.
  */
final case class AsymmetricObjectPropertyAxiom(ope: ObjectPropertyExpression) extends ObjectPropertyAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Subproperties object property chains]] in the OWL 2 specification.
  *
  * @param objectPropertyExpressions  list of object property expressions
  */
final case class ObjectPropertyChain(objectPropertyExpressions: Seq[ObjectPropertyExpression])
  extends SubObjectPropertyExpression

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Anonymous_Individuals Anonymous Individuals]] in the OWL 2 specification.
  *
  * @param nodeId  the nodeID of anonymous individual
  */
final case class AnonymousIndividual(nodeId: String)
  extends Individual with AnnotationValue with AnnotationSubject {
  override def toString: String = nodeId
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Positive_Object_Property_Assertions positive object property assertion axiom]] in the OWL 2 specification.
  */
final case class ObjectPropertyAssertionAxiom(property: ObjectPropertyExpression, subject: Individual, obj: Individual)
  extends AssertionAxiom

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Object_Properties Object Property]] in the OWL 2 specification.
  */
final case class ObjectProperty(override val iri: IRI)
  extends IRIObject(iri) with Entity with ObjectPropertyExpression {
  override def namedProperty: this.type = this
  override def inverse = ObjectInverseOf(this)
  override def isTopObjectProperty: Boolean = iri == IRI.owlTopObjectProperty
}

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Enumeration_of_Individuals Enumeration of Individuals]] in the OWL 2 specification.
  *
  * @param individuals  list of individuals
  */
final case class ObjectOneOf(individuals: Set[Individual]) extends ClassExpression {
  require(individuals.nonEmpty)
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement =
    if (individuals.size == 1) ObjectComplementOf(this)
    else ObjectIntersectionOf(individuals map { ind: Individual => ObjectComplementOf(ObjectOneOf(Set(ind))) })
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality minimum cardinality restriction]] in the OWL 2 specification.
  */
final case class ObjectMinCardinality(cardinality: Int, ope: ObjectPropertyExpression, ce: ClassExpression = owlThing) extends ClassExpression {
  require(cardinality >= 0)
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf: ClassExpression =
    if (cardinality == 0) owlThing
    else ObjectMinCardinality(cardinality, ope, ce.nnf)
  override def nnfComplement: ClassExpression =
    // ¬(>= n R.A) iff <= (n-1) R.A  (but require n > 1 for this to be a legal construction).
    if (cardinality == 0) owlNothing
    else ObjectMaxCardinality(cardinality - 1, ope, ce.nnf)
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality maximum cardinality restriction]] in the OWL 2 specification.
  */
final case class ObjectMaxCardinality(cardinality: Int, ope: ObjectPropertyExpression, ce: ClassExpression = owlThing) extends ClassExpression {
  require(cardinality >= 0)
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = ObjectMaxCardinality(cardinality, ope, ce.nnf)
  override def nnfComplement = ObjectMinCardinality(cardinality + 1, ope, ce.nnf) // ¬(<= n R.A) iff >= (n+1) R.A
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Exact_Cardinality exact cardinality restriction]] in the OWL 2 specification.
  */
final case class ObjectExactCardinality(cardinality: Int, ope: ObjectPropertyExpression, ce: ClassExpression = owlThing) extends ClassExpression {
  require(cardinality >= 0)
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf =
    if (cardinality == 0) ObjectAllValuesFrom(ope, ce.nnfComplement)
    else ObjectExactCardinality(cardinality, ope, ce.nnf)
  override def nnfComplement =
    if (cardinality == 0)
      ObjectSomeValuesFrom(ope, ce.nnf)
    else
      ObjectUnionOf(Set(ObjectMaxCardinality(cardinality - 1, ope, ce), ObjectMinCardinality(cardinality + 1, ope, ce)))
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Annotation_Property_Range Annotation Property Range]] in the OWL 2 specification.
  */
final case class AnnotationPropertyRangeAxiom(property: AnnotationProperty, range: IRI) extends AnnotationAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties Inverse Object Property]] in the OWL 2 specification.
  *
  * @param objectProperty  object property expression that this expression refers to
  */
final case class ObjectInverseOf(objectProperty: ObjectProperty) extends ObjectPropertyExpression {
  override def namedProperty: ObjectProperty = objectProperty
  override def inverse = objectProperty
  override def isTopObjectProperty: Boolean = objectProperty.isTopObjectProperty
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Annotation_Property_Domain Annotation Property Domain]] in the OWL 2 specification.
  */
final case class AnnotationPropertyDomainAxiom(property: AnnotationProperty, domain: IRI) extends AnnotationAxiom

/**
  * Corresponds to an ??? in the OWL 2 specification.
  */
final case class AnnotationProperty(override val iri: IRI) extends IRIObject(iri) with Entity

/** Annotation assertion axiom as defined in [[http://www.w3.org/TR/owl2-syntax/#Annotation_Assertion Section 10.2.1]] of the specification
  *
  * Note that the superclass of AnnotationAssertion is AnnotationAxiom, not AssertionAxiom.
  */
final case class AnnotationAssertionAxiom(property: AnnotationProperty, subject: AnnotationSubject, value: AnnotationValue) extends AnnotationAxiom

/** Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Intersection_of_Class_Expressions Intersection of Class Expressions]] in the OWL 2 specification.
  *
  * @param ces  set of class expressions
  */
final case class ObjectIntersectionOf(ces: Set[ClassExpression]) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = ObjectIntersectionOf(ces map {_.nnf})
  override def nnfComplement = ObjectUnionOf(ces map {_.nnfComplement})
}

/**
  * Corresponds to an [[http://www.w3.org/TR/owl2-syntax/#Individual_Value_Restriction Individual Value Restriction for Object Properties]] in the OWL 2 specification.
  */
final case class ObjectHasValue(ope: ObjectPropertyExpression, individual: Individual) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement = ObjectAllValuesFrom(ope, ObjectComplementOf(ObjectOneOf(Set(individual))))
}

/**
  * Corresponds to a [[http://www.w3.org/TR/owl2-syntax/#Self-Restriction Self-Restriction]] in the OWL 2 specification.
  */
final case class ObjectHasSelf(ope: ObjectPropertyExpression) extends ClassExpression {
  override def isThing: Boolean = false
  override def isNothing: Boolean = false
  override def nnf = this
  override def nnfComplement = ObjectComplementOf(this)
}
