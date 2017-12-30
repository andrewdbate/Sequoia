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
import com.sequoiareasoner.kernel.index.AnyRefUnionFind
import com.sequoiareasoner.kernel.owl.PredefinedOWLClass._
import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.owl.model._
import com.sequoiareasoner.kernel.structural.UnsupportedFeatureObserver
import com.sequoiareasoner.kernel.structural.chains.AutomatonBuilder._
import com.sequoiareasoner.arrayops._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Transforms a collection of axioms by encoding away property chains using automata. The resulting set of axioms does
  * not contain any property chains.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param axioms           the axioms to transform.
  * @param featureObserver  the observer to call when unsupported logical features are encountered in the input axioms.
  */
class PropertyChainEncoder(axioms: Iterable[Axiom], featureObserver: UnsupportedFeatureObserver) {

  // FIXME: handle owl:topObjectProperty

  private[this] val simplicityChecker = new SimplePropertyRestrictionValidator(axioms)

  /** Input axioms with EquivalentObjectPropertiesAxiom, InverseObjectPropertiesAxiom and SymmetricObjectPropertyAxiom
    * removed.
    */
  private[this] val filteredAxioms = new ArrayBuffer[Axiom]

  /** `knownPropertyInclusionGraph` has an edge from `A` to `B` if `B` is a known super property expression of `A`. */
  private[this] val knownPropertyInclusionGraph = new MutableGraph[ObjectPropertyExpression]
  for (ax <- axioms) ax match {
    case EquivalentObjectPropertiesAxiom(properties) =>
      val propertiesSeq = properties.toArray
      val length = propertiesSeq.length
      crange (0, length) { i => crange(i + 1, length) { j =>
        knownPropertyInclusionGraph.addEdge(propertiesSeq(i), propertiesSeq(j))
        knownPropertyInclusionGraph.addEdge(propertiesSeq(i).inverse, propertiesSeq(j).inverse)
        knownPropertyInclusionGraph.addEdge(propertiesSeq(j), propertiesSeq(i))
        knownPropertyInclusionGraph.addEdge(propertiesSeq(j).inverse, propertiesSeq(i).inverse)
      } }
    case InverseObjectPropertiesAxiom(first, second) =>
      knownPropertyInclusionGraph.addEdge(first, second.inverse)
      knownPropertyInclusionGraph.addEdge(first.inverse, second)
      knownPropertyInclusionGraph.addEdge(second, first.inverse)
      knownPropertyInclusionGraph.addEdge(second.inverse, first)
    case SubObjectPropertyOfAxiom(lhs: ObjectPropertyExpression, rhs) =>
      knownPropertyInclusionGraph.addEdge(lhs, rhs)
      knownPropertyInclusionGraph.addEdge(lhs.inverse, rhs.inverse)
      filteredAxioms += ax
    case SymmetricObjectPropertyAxiom(ope) =>
      knownPropertyInclusionGraph.addEdge(ope, ope.inverse)
      knownPropertyInclusionGraph.addEdge(ope.inverse, ope)
    case _: DeclarationAxiom =>
      // Declarations do not affect reasoning, so discard.
    case _: AnnotationAxiom =>
      // Annotations do not affect reasoning, so discard.
    case _ =>
      filteredAxioms += ax
  }

  private[this] val equivalentProperties: AnyRefUnionFind[ObjectPropertyExpression] =
    knownPropertyInclusionGraph.stronglyConnectedComponents

  private[this] def canonical(ope: ObjectPropertyExpression): ObjectPropertyExpression = {
    val opeRep: ObjectPropertyExpression = equivalentProperties.getRepresentative(ope)
    val opeInvRep: ObjectPropertyExpression = equivalentProperties.getRepresentative(ope.inverse)
    val opeRepIri: IRI = opeRep.namedProperty.iri
    val opeRepInvIri: IRI = opeInvRep.namedProperty.iri
    if (opeRepIri <= opeRepInvIri) opeRep else opeInvRep.inverse
  }

  /** Returns `true` iff `ope1` and `ope2` are equivalent in the reflexive-transitive closure of the property hierarchy.
    *
    * @param ope1
    * @param ope2
    * @return
    */
  private[this] def areEquivalent(ope1: ObjectPropertyExpression, ope2: ObjectPropertyExpression): Boolean = {
    val ope1Rep = equivalentProperties.getRepresentative(ope1)
    val ope2Rep = equivalentProperties.getRepresentative(ope2)
    ope1Rep == ope2Rep
  }

  /** Returns `true` iff `op` is found to be symmetric in the reflexive-transitive closure of the property hierarchy.
    *
    * @param op
    * @return
    */
  private[this] def isSymmetric(op: ObjectProperty): Boolean = areEquivalent(op, op.inverse)

  private[this] val simpleSubProperties = new ArrayBuffer[SubObjectPropertyOfAxiom[ObjectPropertyExpression]]
  private[this] val riasForAutomatonBuilder = new ArrayBuffer[RoleInclusionAxiom]
  private[this] val axiomsToProcess = new ArrayBuffer[Axiom]

  for (ax <- filteredAxioms) ax match {
    case SubObjectPropertyOfAxiom(ope1: ObjectPropertyExpression, ope2) =>
      if (!areEquivalent(ope1, ope2)) {
        if (simplicityChecker.isSimple(ope2))
          simpleSubProperties += SubObjectPropertyOfAxiom(canonical(ope1), canonical(ope2))
        else
          riasForAutomatonBuilder += RoleInclusionAxiom(Seq(canonical(ope1)), canonical(ope2))
      }
    case SubObjectPropertyOfAxiom(ObjectPropertyChain(chain), ope) =>
      riasForAutomatonBuilder += RoleInclusionAxiom(chain map canonical, canonical(ope))
    case TransitiveObjectPropertyAxiom(property) =>
      riasForAutomatonBuilder += RoleInclusionAxiom(Seq(canonical(property), canonical(property)), canonical(property))
    case _ =>
      axiomsToProcess += ax
  }

  /** Construct the automaton for the RIAs. */
  private[this] val builder = new AutomatonBuilder(riasForAutomatonBuilder, isSymmetric)

  /** The set of internalised GCIs in negation normal form generated. */
  private[this] val internalizedAxioms = new ArrayBuffer[(ClassExpression,ClassExpression)]
  private[this] val asymmetricProperties = new ArrayBuffer[AsymmetricObjectPropertyAxiom]
  private[this] val reflexiveProperties = new ArrayBuffer[ReflexiveObjectPropertyAxiom]
  private[this] val disjointProperties = new ArrayBuffer[DisjointObjectPropertiesAxiom]

  /** Returns the encoding of a property that may occur on the right of a property chain.
    *
    * Given the non-deterministic finite automaton A = (states, symbols, transition, initials, terminals) that
    * corresponds to an object property expression, the set of clauses is constructed as follows:
    * For each transition (u, R, v), we introduce the clause:
    *   AUX_u -> \ForAll R AUX_v
    *
    * Each initial state i corresponds to the negative concept AUX_i, and each terminal state t corresponds to the
    * positive concept AUX_t.
    *
    * @param ope
    * @param ce a class expression that has _already_ been transformed
    * @return
    */
  private[this] def rewriteForAll(ope: ObjectPropertyExpression,
                                  ce: ClassExpression): ClassExpression = {
    /* OPEs in RIAs are replaced with representatives of the equivalence class, so get the NFA for the representative,
     * but we do not need to get the canonical OPEs for transition labels since they were substituted before. */
    val nfa: NFA = builder.getAutomaton(canonical(ope))

    // true if the auxiliary concept that corresponds to an initial state would only occur negatively if generated.
    val canInlineInitialStates = nfa.delta forall {
      case Transition(_, _, sink) => !sink.isInitial
    }

    // true if the auxiliary concept that corresponds to a terminal state would only occur positively if generated.
    val canInlineTerminalStates = nfa.delta forall {
      case Transition(source, _, _) => !source.isTerminal
    }

    @inline def genAuxForState(s: State): Boolean =
      (s.isInitial && !canInlineInitialStates) ||
      (s.isTerminal && !canInlineTerminalStates) ||
      (!s.isInitial && !s.isTerminal)

    val flipPolarityOfAux = true

    val stateMap = new mutable.AnyRefMap[State, OWLClass]
    for (s: State <- nfa.states)
      if (genAuxForState(s)) stateMap.put(s, OWLClass(IRI.all()))

    @inline def ceForState(s: State, negativePolarity: Boolean): ClassExpression =
      if (s.isTerminal && canInlineTerminalStates) ce
      else if (negativePolarity ^ flipPolarityOfAux) ObjectComplementOf(stateMap(s))
      else stateMap(s)

    val classExpressions = new ArrayBuffer[ClassExpression]
    for (Transition(source, label, sink) <- nfa.delta) {
      val expressionForTransition =
        if (label ne null) ObjectAllValuesFrom(label, ceForState(sink, false))
        else ceForState(sink, false)

      if (source.isInitial) {
        if (canInlineInitialStates)
          classExpressions += expressionForTransition
        else
          classExpressions += ceForState(source, false)
      }
      if (!source.isInitial || !canInlineInitialStates)
        internalizedAxioms += ((ceForState(source, true), expressionForTransition))
    }

    if (!canInlineTerminalStates)
      for (s: State <- nfa.terminals)
        internalizedAxioms += ((ceForState(s, true), ce))

    // Conjunction over the initial transitions.
    ObjectIntersectionOf(classExpressions.toSet)
  }

  /** ObjectMaxCardinality(n R A) will be transformed into clauses as
    * NOT(R(x, z0)) OR ... OR NOT(R(x, zn)) OR NOT(A(z0)) OR ... OR NOT(A(zn))
    * and thus the filler will be complemented. This will introduce universal quantifiers if existential quantifiers
    * occur in `ce`.
    *
    * @param n
    * @param ope
    * @param ce
    * @return
    */
  private[this] def atMost(n: Int, ope: ObjectPropertyExpression, ce: ClassExpression): ClassExpression =
    ce match {
      case cr: OWLClass =>
        ObjectMaxCardinality(n, canonical(ope), cr)
      case oco @ ObjectComplementOf(_: OWLClass) =>
        ObjectMaxCardinality(n, canonical(ope), oco)
      case _ =>
        val aux = OWLClass(IRI.all())
        internalizedAxioms += ((transform(ce.nnfComplement), aux))
        ObjectMaxCardinality(n, canonical(ope), aux)
    }

  /** Given a class expression in negation-normal form, returns a class expression in negation-normal form which has the
    * property inclusions encoded.
    *
    * Note that even if the input class expression is in conjunctive-normal form, the result is not guaranteed to be in
    * conjunctive-formal form.
    *
    * @param ceNNF  class expression in negation formal form
    * @return
    */
  private[this] def transform(ceNNF: ClassExpression): ClassExpression = ceNNF match {
    case cr: OWLClass =>
      cr
    case oco @ ObjectComplementOf(_: OWLClass) =>
      oco
    case oco @ ObjectComplementOf(_: ObjectHasSelf) =>
      oco
    case oco @ ObjectComplementOf(_: ObjectOneOf) =>
      oco
    case ObjectAllValuesFrom(ope, ce) =>
      rewriteForAll(ope, transform(ce))
    case ObjectExactCardinality(n, ope, ce) =>
      ObjectIntersectionOf(Set(ObjectMinCardinality(n, canonical(ope), transform(ce)), atMost(n, ope, ce)))
    case ObjectIntersectionOf(ces) =>
      ObjectIntersectionOf(ces map transform)
    case ObjectMaxCardinality(n, ope, ce) =>
      atMost(n, ope, ce)
    case ObjectMinCardinality(n, ope, ce) =>
      ObjectMinCardinality(n, canonical(ope), transform(ce))
    case ObjectSomeValuesFrom(ope, ce) =>
      ObjectSomeValuesFrom(canonical(ope), transform(ce))
    case ObjectUnionOf(ces) =>
      ObjectUnionOf(ces map transform)
    case ohs: ObjectHasSelf =>
      ohs
    case ObjectHasValue(ope, individual) =>
      ObjectHasValue(canonical(ope), individual)
    case ooo: ObjectOneOf =>
      ooo
    case dav: DataAllValuesFrom => dav
    case dec: DataExactCardinality => dec
    case dhv: DataHasValue => dhv
    case dmc: DataMaxCardinality => dmc
    case dmc: DataMinCardinality => dmc
    case dsv: DataSomeValuesFrom => dsv
    case ce: ObjectComplementOf =>
      throw new AssertionError(s"$ce is not in NNF.")
  }

  /**
    * @param axiom
    * @return the right hand side of an internalised GCI in NNF
    */
  private[this] def internalize(axiom: Axiom): Unit = {
    axiom match {
      case DisjointClassesAxiom(ces) =>
        simplicityChecker.isValid(axiom)
        // For each class expression that is repeated, that class expression is equivalent to owl:Nothing.
        val repetitions: Set[ClassExpression] = ces.groupBy(identity).collect{ case (x, ys) if ys.size > 1 => x }.toSet
        // Otherwise, transform Disjoint(A B) as A \wedge B -> \bottom for each pair A and B in ces -- repetitions.
        val others: Array[ClassExpression] = (ces.toSet -- repetitions).toArray
        for (ce <- repetitions)
          internalizedAxioms += ((transform(ce.nnfComplement), owlNothing))
        crange (0, others.length) { i => crange(i + 1, others.length) { j =>
          internalizedAxioms += ((transform(others(i).nnfComplement), transform(others(j).nnfComplement)))
        } }
      case DisjointUnionAxiom(definedClass, ces) =>
        simplicityChecker.isValid(axiom)
        ???
      case EquivalentClassesAxiom(ces) =>
        simplicityChecker.isValid(axiom)
        val prop: Array[ClassExpression] = ces.toArray
        if (prop.length > 1)
          for (i <- 0 until prop.length) {
            val lhs = prop(i)
            val rhs = prop((i + 1) % prop.length)
            internalizedAxioms += ((transform(lhs.nnfComplement), transform(rhs.nnf)))
          }
      case HasKeyAxiom(ce, opes, dpes) =>
        simplicityChecker.isValid(axiom)
        ???
      case SubClassOfAxiom(lhs, rhs) =>
        simplicityChecker.isValid(axiom)
        internalizedAxioms += ((transform(lhs.nnfComplement), transform(rhs.nnf)))
      case ObjectPropertyDomainAxiom(ope, domain) =>
        simplicityChecker.isValid(axiom)
        // Syntactic shortcut for SubClassOf(ObjectSomeValuesFrom(ope owl:Thing) domain).
        internalizedAxioms += ((transform(domain.nnf), rewriteForAll(ope, owlNothing)))
      case ObjectPropertyRangeAxiom(ope, range) =>
        simplicityChecker.isValid(axiom)
        // Syntactic shortcut for SubClassOf(owl:Thing ObjectAllValuesFrom(ope range)).
        internalizedAxioms += ((owlNothing, rewriteForAll(ope, transform(range.nnf))))
      case FunctionalObjectPropertyAxiom(ope) =>
        simplicityChecker.isValid(axiom)
        // Syntactic shortcut for SubClassOf(owl:Thing ObjectMaxCardinality(1 ope)).
        internalizedAxioms += ((owlNothing, ObjectMaxCardinality(1, canonical(ope))))
      case InverseFunctionalObjectPropertyAxiom(ope) =>
        simplicityChecker.isValid(axiom)
        // Syntactic shortcut for SubClassOf(owl:Thing ObjectMaxCardinality(1 ObjectInverseOf(ope))).
        internalizedAxioms += ((owlNothing, ObjectMaxCardinality(1, canonical(ope.inverse))))
      case AsymmetricObjectPropertyAxiom(ope) =>
        simplicityChecker.isValid(axiom)
        asymmetricProperties += AsymmetricObjectPropertyAxiom(canonical(ope))
      case ReflexiveObjectPropertyAxiom(ope) =>
        simplicityChecker.isValid(axiom)
        reflexiveProperties += ReflexiveObjectPropertyAxiom(canonical(ope))
      case DisjointObjectPropertiesAxiom(opes) =>
        simplicityChecker.isValid(axiom)
        disjointProperties += DisjointObjectPropertiesAxiom(opes map canonical)
      case ax: IrreflexiveObjectPropertyAxiom =>
        simplicityChecker.isValid(axiom)
        featureObserver.reportUnsupported(ax)
      case ax: DatatypeDefinitionAxiom =>
        featureObserver.reportUnsupported(ax)
      case ax: DataPropertyDomainAxiom =>
        simplicityChecker.isValid(axiom)
        featureObserver.reportUnsupported(ax)
      case ax: DataPropertyAxiom =>
        featureObserver.reportUnsupported(ax)
      case ax: ClassAssertionAxiom =>
        simplicityChecker.isValid(axiom)
        featureObserver.reportUnsupported(ax)
      case ax: AssertionAxiom =>
        featureObserver.reportUnsupported(ax)
      case _: DeclarationAxiom =>
        throw new Error("DeclarationAxiom should have been discarded by the preprocessing phase.")
      case _: AnnotationAxiom =>
        throw new Error("AnnotationAxiom should have been discarded by the preprocessing phase.")
      case _: EquivalentObjectPropertiesAxiom =>
        throw new Error("EquivalentObjectPropertiesAxiom should have been handled by the preprocessing phase.")
      case _: InverseObjectPropertiesAxiom =>
        throw new Error("InverseObjectPropertiesAxiom should have been handled by the preprocessing phase.")
      case _: SubObjectPropertyOfAxiom[_] =>
        throw new Error("SubObjectPropertyOfAxiom should have been handled by the automaton encoding phase.")
      case _: SymmetricObjectPropertyAxiom =>
        throw new Error("SymmetricObjectPropertyAxiom should have been handled by the automaton encoding phase.")
      case _: TransitiveObjectPropertyAxiom =>
        throw new Error("TransitiveObjectPropertyAxiom should have been handled by the automaton encoding phase.")
    }
  }

  // This gets processed in the constructor.
  for (ax <- axiomsToProcess) internalize(ax)

  /** Returns the collection of binary disjunctions that encode the internalised axioms of the input ontology, excluding
    * any asymmetry axioms, disjointness axioms, and any simple sub-property inclusions.
    *
    * @return collection of binary disjunctions that encode the internalised axioms of the input ontology.
    */
  def getInternalizedAxioms: Iterator[(ClassExpression, ClassExpression)] = internalizedAxioms.iterator

  // TODO: update documentation that these are the canonical properties.

  /**
    * @return the asymmetric object property axioms of the input ontology.
    */
  def getAsymmetricProperties: Iterator[AsymmetricObjectPropertyAxiom] = asymmetricProperties.iterator

  /**
    * @return the reflexive object property axioms of the input ontology.
    */
  def getReflexiveProperties: Iterator[ReflexiveObjectPropertyAxiom] = reflexiveProperties.iterator

  /**
    * @return the disjoint object properties axioms of the input ontology.
    */
  def getDisjointProperties: Iterator[DisjointObjectPropertiesAxiom] = disjointProperties.iterator

  /** Returns the simple sub property inclusions of the input ontology. These are object property inclusions of the form
    * OPE1 IS-A OPE2, where both OPE1 and OPE2 are simple object property expressions.
    *
    * @return the simple sub property inclusions of the input ontology.
    */
  def getSimpleSubProperties: Iterator[SubObjectPropertyOfAxiom[ObjectPropertyExpression]] = simpleSubProperties.iterator

  override def toString: String = knownPropertyInclusionGraph.toString

}
