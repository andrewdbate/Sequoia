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

import com.sequoiareasoner.kernel.clauses.Term._
import com.sequoiareasoner.kernel.clauses.{Concept, Equality, Inequality, OntologyClause, Predicate, Role, STClause, Substitution, Term, TermSubstitution, Literal => DLLiteral}
import com.sequoiareasoner.kernel.index.{CollectionMakeString, ImmutableSet, IntSet, TotalIRIMultiMap}
import com.sequoiareasoner.kernel.owl.PredefinedOWLClass._
import com.sequoiareasoner.kernel.owl.iri._
import com.sequoiareasoner.kernel.owl.model._
import com.sequoiareasoner.kernel.structural.chains.PropertyChainEncoder
import com.sequoiareasoner.arrayops._

import scala.collection.mutable

object DLOntology {

  private[this] var varIdCount: Int = _
  private[this] var functionCount: Int = _
  resetTerms

  private def resetTerms: Unit = {
    varIdCount = 1
    functionCount = 1
  }

  /**
   * @return a fresh z neighbour variable
   */
  private def zFresh(): Term = {
    val zIndex = varIdCount
    varIdCount += 1
    z(zIndex)
  }

  private def zFresh(num: Int): Array[Term] = {
    val zIndex = varIdCount
    varIdCount += 1
    cyield (0, num) { i => z(zIndex + i) }
  }


  /**
    * @return an array of fresh terms of the form f(x) where f is a fresh symbol
    */
  private def fFresh(num: Int): Array[Term] = {
    val fIndex = functionCount
    functionCount += num
    cyield (0, num) { i => f(fIndex + i) }
  }

  @inline def Body(ps: Predicate*) = Array(ps: _*)
  @inline def Head(ls: DLLiteral*) = Array(ls: _*)
}

/** Implements structural transformation. The result is a collection of ontology clauses.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param axioms           the axioms to structurally transform into ontology clauses.
  * @param featureObserver  the observer to call when unsupported logical features are encountered in the input axioms.
  */
class DLOntology(axioms: Iterable[Axiom], featureObserver: UnsupportedFeatureObserver) extends CollectionMakeString {
  // Must reset term counts to avoid overflow when multiple DLOntology objects are created within an application.
  DLOntology.resetTerms

  import DLOntology._

  private[this] implicit val self = this

  private[this] val makeElementData: Int => Array[OntologyClause] = new Array[OntologyClause](_)
  /** facts is the set of clauses with an empty body. */
  private[this] val facts = new mutable.HashSet[OntologyClause]
  /** conceptClauses maps each concept UID C to the set of clauses that contain C(x) in the body. */
  private[this] val conceptClauses = new TotalIRIMultiMap(makeElementData)
  /** forwardRoleClauses maps each role UID R to the set of clauses that, for some i, contain R(x, z_i) in the body. */
  private[this] val forwardRoleClauses = new TotalIRIMultiMap(makeElementData)
  /** backwardRoleClauses maps each role UID R to the set of clauses that, for some i, contain R(z_i, x) in the body. */
  private[this] val backwardRoleClauses = new TotalIRIMultiMap(makeElementData)
  /** conceptTriggers contains A if A(x) occurs in the body of some clause. */
  private[this] val conceptSuccTriggers = new mutable.BitSet
  /** roleTriggers contains R if, for some i, we have R(x,z_i) occurs in the body of some clause. */
  private[this] val forwardRoleSuccTriggers = new mutable.BitSet
  /** inverseRoleTriggers contains R if, for some i, we have R(z_i,x) occurs in the body of some clause. */
  private[this] val backwardRoleSuccTriggers = new mutable.BitSet
  /** conceptsToClassify is the set of concepts whose IRIs are not internal and that appear in the body of some ontology clause.
    * These are the concepts that need to be classified to determine the full classification.
    * INVARIANT: conceptsToClassify map { _.iri} subsetOf conceptTriggers */
  private[this] val conceptsToClassify = new mutable.HashSet[Concept]

  conceptsToClassify += Concept(IRI.owlThing, x) // TODO: owl:Thing this should be moved to the inconsistency check.


  private[this] val functionTermMap = new mutable.LongMap[ImmutableSet[Predicate]]

  def getKnownPredicates(t: Term): ImmutableSet[Predicate] = {
    assert(t.isFunctionTerm)
    val value = functionTermMap.getOrNull(t.id)
    if (value eq null) ImmutableSet.empty else value
  }

  private[this] def putKnownPredicate(t: Term, p: Predicate): Unit = {
    val existing = functionTermMap.getOrNull(t.id)
    if (existing ne null) functionTermMap.put(t.id, existing + p)
    else functionTermMap.put(t.id, ImmutableSet(p))
  }

  private[this] val unsatisfiablePredicates = new IntSet

  /**
    * @param l the literal to test
    * @return `true` if and only if the literal `l` is asserted to be equivalent to `owl:Nothing` in the ontology
    */
  def isNothing(l: DLLiteral): Boolean = l match {
    case p: Predicate => unsatisfiablePredicates contains p.iri.uid
    case _ => false
  }

  private[this] def indexOntologyClause(c: OntologyClause): Unit = {
    val body = c.body
    if (body.length == 0) {
      // c is a Horn fact
      facts += c
    } else {
      if (body.length == 1 && c.head.length == 0) {
        unsatisfiablePredicates += body(0).iri.uid
      }
      cforeach (body) {
        case p@Concept(iri, `x`) if iri.isImported =>
          conceptsToClassify += p
          conceptSuccTriggers(iri.uid) = true
          conceptClauses.addBinding(iri, c)
        case Concept(iri, `x`) =>
          conceptSuccTriggers(iri.uid) = true
          conceptClauses.addBinding(iri, c)
        case Role(iri, `x`, _) =>
          forwardRoleSuccTriggers(iri.uid) = true
          forwardRoleClauses.addBinding(iri, c)
        case Role(iri, _, `x`) =>
          backwardRoleSuccTriggers(iri.uid) = true
          backwardRoleClauses.addBinding(iri, c)
        case _ => throw new Error(s"Illegal clause $c. Concepts containing z_i are not allowed to appear in the body.")
      }
    }
  }

  def getFacts: Iterator[OntologyClause] = facts.iterator

  /** Returns an iterator over the ontology clauses that contain a predicate in the body that can unify with the
    * specified predicate using a central substitution.
    *
    * @param p
    * @return
    */
  def getClauses(p: Predicate): Iterator[OntologyClause] =
    p match {
      case Concept(iri, _) => conceptClauses(iri).iterator
      case Role(iri, s, t) if s.isCentralVariable && t.isCentralVariable => forwardRoleClauses(iri).iterator ++ backwardRoleClauses(iri).iterator
      case Role(iri, s, _) if s.isCentralVariable => forwardRoleClauses(iri).iterator
      case Role(iri, _, t) if t.isCentralVariable => backwardRoleClauses(iri).iterator
    }

  def isConceptSuccTrigger(c: IRI): Boolean = conceptSuccTriggers(c.uid)
  def isForwardRoleSuccTrigger(r: IRI): Boolean = forwardRoleSuccTriggers(r.uid)
  def isBackwardRoleSuccTrigger(r: IRI): Boolean = backwardRoleSuccTriggers(r.uid)

  def getConceptsToClassify: Set[Concept] = conceptsToClassify.toSet

  /** Return all ontology clauses in this DL ontology. This method is intended for testing only and it provides no
    * guarantees that the set of clauses will be obtained efficiently.
    *
    * @return all ontology clauses in this DL ontology
    */
  def getOntologyClauses: Iterator[OntologyClause] =
    facts.iterator ++ conceptClauses.valuesIterator ++ forwardRoleClauses.valuesIterator ++ backwardRoleClauses.valuesIterator

  // --------------------- The methods below this line implement the structural transformation --------------------- //

  private[this] val clauseTransformer = new STClauseToOntologyClauseTransformer

  private[this] def addToIndex(c: STClause): Unit = {
    val result = clauseTransformer.transform(c)
    for (c <- result) indexOntologyClause(c)
  }

  private[this] def addToIndex(cs: Iterable[STClause]): Unit = {
    val result = clauseTransformer.transform(cs)
    for (c <- result) indexOntologyClause(c)
  }

  /** Given an object property expression, obtain the corresponding role in first-order syntax. This method does not
    * construct the encoding of the property chains using the automaton, and hence it is intended to be used only when
    * handling exact, min and max cardinality expressions.
    *
    * @param ope
    * @return
    */
  private[this] def getRole(term: Term)(ope: ObjectPropertyExpression): Role =
    ope match {
      case ObjectProperty(iri) =>
        Role(iri, x, term)
      case ObjectInverseOf(ObjectProperty(iri)) =>
        Role(iri, term, x)
    }

  private type ClauseFragment = (Seq[Predicate], Seq[DLLiteral])

  private[this] def combine(doCrossProduct: Boolean)(l: Set[ClauseFragment], r: Set[ClauseFragment]): Set[ClauseFragment] =
    if (doCrossProduct)
      // Convert disjunction of CNF clauses to CNF
      for ((lBody, lHead) <- l; (rBody, rHead) <- r) yield (lBody ++ rBody, lHead ++ rHead)
    else
      l ++ r

  private[this] def predicateSubstitution(sigma: Substitution)(l: Predicate): Predicate = l applySubstitution sigma

  private[this] def literalSubstitution(sigma: Substitution)(l: DLLiteral): DLLiteral = l applySubstitution sigma

  private[this] def noNeighbourTerm(l: DLLiteral): Boolean = l match {
    case Concept(_, t)   => t.isCentralVariable
    case _               => false
  }

  private[this] val transformedInput = new PropertyChainEncoder(axioms, featureObserver)

  /** Generates a universal corresponding to a _simple_ object property expression.
    *
    * {{{
    * A -> \forall R . C  becomes  A(x) \wedge R(x,z1) -> C(z1)
    * }}}
    *
    * @param ope
    * @param ce
    * @return
    */
  private[this] def forAll(ope: ObjectPropertyExpression, ce: ClassExpression): Set[ClauseFragment] = {
    val zSymbol: Term = zFresh()
    val role: Predicate = getRole(zSymbol)(ope)
    val fragmentsFromFiller: Set[ClauseFragment] = normalise(ce)

    val canInlineFiller = fragmentsFromFiller forall {
      case (body, head) => body.forall(noNeighbourTerm) && head.forall(noNeighbourTerm)
    }

    if (canInlineFiller) {
      // Optimization.

      val sigma = new TermSubstitution(zSymbol)
      for ((body, head) <- fragmentsFromFiller) yield {
        val sigmaBody: Seq[Predicate] = body map predicateSubstitution(sigma)
        val sigmaHead: Seq[DLLiteral] = head map literalSubstitution(sigma)
        (role +: sigmaBody, sigmaHead)
      }

    } else {

      // Generate auxiliary clauses.
      val auxIri = IRI.all()
      val auxConceptNeighbour: Predicate = Concept(auxIri, zSymbol)

      val auxConceptCentral = Concept(auxIri, x)
      // Try to reduce the total number of disjuncts in the set of clauses.
      val negateAux = fragmentsFromFiller forall { case (_, head) => head.isEmpty }

      if (negateAux)
        for ((body, head) <- fragmentsFromFiller)
          addToIndex(STClause(body, auxConceptCentral +: head))
      else
        for ((body, head) <- fragmentsFromFiller)
          addToIndex(STClause(auxConceptCentral +: body, head))

      val fragment: ClauseFragment =
        if (negateAux) (Seq(role, auxConceptNeighbour), Nil)
        else (Seq(role), Seq(auxConceptNeighbour))

      Set(fragment)
    }
  }

  /**
    * {{{
    * A -> <= n R . C     becomes  A(x) \bigwedge_{i=0}^n (R(x,z_i) \wedge C(z_i)) -> \bigvee_{0<=i<j<=n} z_i = z_j
    * }}}
    *
    * PRECONDITION: `ope` is a simple object property expression.
    *
    * @param ope
    * @param ce
    */
  private[this] def atMost(n: Int,
                           ope: ObjectPropertyExpression,
                           ce: ClassExpression): Set[ClauseFragment] = {
    require(n >= 0)

    val zSymbols: Array[Term] = zFresh(n + 1)

    val roles: Seq[Predicate] =
      cyield (0, n + 1) { i => getRole(zSymbols(i))(ope) }.toSeq
    val equalities: Seq[Equality] =
      cyield (0, n + 1, {i => i + 1}, {_ => n + 1}) { (i, j) => Equality(zSymbols(i), zSymbols(j)) }.toSeq

    val fragmentsFromFiller: Set[ClauseFragment] = ce match {
      case cr: OWLClass                     => normalise(ObjectComplementOf(cr))
      case ObjectComplementOf(cr: OWLClass) => normalise(cr)
      case _                                => throw new AssertionError(s"$ce is not in a valid normal form.")
    }

    val zFragments: Seq[Set[ClauseFragment]] =
      cyield (0, n + 1) { i =>
        val sigma = new TermSubstitution(zSymbols(i))
        for ((body, head) <- fragmentsFromFiller) yield {
          val sigmaBody: Seq[Predicate] = body map predicateSubstitution(sigma)
          val sigmaHead: Seq[DLLiteral] = head map literalSubstitution(sigma)
          (sigmaBody, sigmaHead)
        }
      }

    zFragments reduce combine(doCrossProduct = true) map {
      case (body, head) => (body ++ roles, head ++ equalities)
    }

  }

  /**
    * {{{
    * A -> \exists R . C  becomes  A(x) -> R(x,f(x))  and  A(x) -> C(f(x))
    * A -> >= n R . C     becomes  A(x) -> R(x,f_i(x))  and  A(x) -> C(f_i(x)) for 1 <= i <= n
    *                     and      A(x) -> f_i(x) \neq f_j(x) for 1 <= i < j <= n
    * }}}
    *
    * @param ope
    * @param ce
    * @param n
    */
  private[this] def atLeast(n: Int,
                            ope: ObjectPropertyExpression,
                            ce: ClassExpression): Set[ClauseFragment] = {
    require(n > 0)

    val funSymbols: Array[Term] = fFresh(n)

    // Generate inequalities fragments
    val inequalities: Seq[ClauseFragment] =
      cyield (0, n, {i => i + 1}, {_ => n}) { (i, j) => (Nil, Seq(Inequality(funSymbols(j), funSymbols(i)))) }.toSeq

    val fragmentsFromFiller: Set[ClauseFragment] = normalise(ce)

    lazy val auxFillerSuccessor = Concept(IRI.some(), x)

    val allFillers = new mutable.HashSet[Concept]

    for (fragment <- fragmentsFromFiller) fragment match {
      case (Nil, Seq(c @ Concept(iri, t))) if t.isCentralVariable =>
        crange (0, n) { i =>
          putKnownPredicate(funSymbols(i), c)
          allFillers += Concept(iri, funSymbols(i))
        }
      case (body, head) =>
        addToIndex(STClause(auxFillerSuccessor +: body, head))
        crange (0, n) { i =>
          putKnownPredicate(funSymbols(i), auxFillerSuccessor)
          allFillers += auxFillerSuccessor.applySubstitution(new TermSubstitution(funSymbols(i)))
        }
    }

    val (existentialRoles: Seq[ClauseFragment], knownRole: Role) = ope match {
      case ObjectProperty(iri) =>
        (cyield (0, n) { i => (Nil, Seq(Role(iri, x, funSymbols(i)))) }.toSeq, Role(iri, y, x))
      case ObjectInverseOf(ObjectProperty(iri)) =>
        (cyield (0, n) { i => (Nil, Seq(Role(iri, funSymbols(i), x))) }.toSeq, Role(iri, x, y))
    }

    crange (0, n) { i => putKnownPredicate(funSymbols(i), knownRole) }

    val existentialFillers: Set[ClauseFragment] = allFillers.toSet map {
      c: Concept => (Nil, Seq(c))
    }

    existentialFillers ++ existentialRoles ++ inequalities
  }

  private[this] val interning = new mutable.AnyRefMap[ClassExpression, Set[ClauseFragment]]

  private[this] def normalise(ce: ClassExpression): Set[ClauseFragment] =
    interning.getOrElseUpdate(ce, ce match {
      case OWLClass(iri) =>
        // TODO: add tests for this case
        if (iri.isNothing) Set((Nil, Nil))
        else if (iri.isThing) Set.empty
        else Set((Nil, Seq(Concept(iri, x))))

      case ObjectComplementOf(OWLClass(iri)) =>
        // Since `ce` is in NNF, ObjectComplementOf can only occur immediately before a OWLClass.
        if (iri.isNothing) Set.empty
        else if (iri.isThing) Set((Nil, Nil))
        else Set((Seq(Concept(iri, x)), Nil))

      case ObjectComplementOf(ObjectHasSelf(ObjectProperty(iri))) =>
        Set((Seq(Role(iri, x, x)), Nil))

      case ObjectComplementOf(ObjectHasSelf(ObjectInverseOf(ObjectProperty(iri)))) =>
        Set((Seq(Role(iri, x, x)), Nil))

      case ObjectComplementOf(_: ObjectOneOf) =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case ObjectIntersectionOf(ces) =>

        val cesNormalised: Set[Set[ClauseFragment]] = ces map normalise

        cesNormalised reduce {
          (fragments1: Set[ClauseFragment], fragments2: Set[ClauseFragment]) =>
            combine(doCrossProduct = false)(fragments1, fragments2)
        }

      case ObjectUnionOf(ces) =>

        val cesNormalised: Set[Set[ClauseFragment]] =
          ces map {
            ce: ClassExpression =>
              val fragments: Set[ClauseFragment] = normalise(ce)
              val shouldInline = fragments.size <= 1 || fragments.forall {
                case (body, head) => head.isEmpty
              }
              if (shouldInline) {
                fragments
              } else {
                val aux = Concept(IRI.disjunct(), x)
                fragments map {
                  case (body, head) =>
                    addToIndex(STClause(aux +: body, head))
                    (Nil, Seq(aux)): ClauseFragment
                }
              }
          }

        cesNormalised reduce {
          (fragments1: Set[ClauseFragment], fragments2: Set[ClauseFragment]) =>
            combine(doCrossProduct = true)(fragments1, fragments2)
        }


      case ObjectSomeValuesFrom(ope, ce) =>
        atLeast(1, ope, ce)

      case ObjectAllValuesFrom(ope, ce) =>
        forAll(ope, ce)

      case ObjectExactCardinality(n, ope, ce) if n > 0 =>
        val fragments1 = atLeast(n, ope, ce)
        val fragments2 = atMost(n, ope, ce)
        combine(doCrossProduct = false)(fragments1, fragments2)

      case ObjectMaxCardinality(n, ope, ce) =>
        atMost(n, ope, ce)

      case ObjectMinCardinality(n, ope, ce) if n > 0 =>
        atLeast(n, ope, ce)

      case ObjectHasSelf(ObjectProperty(iri)) =>
        Set((Nil, Seq(Role(iri, x, x))))

      case ObjectHasSelf(ObjectInverseOf(ObjectProperty(iri))) =>
        Set((Nil, Seq(Role(iri, x, x))))

      case _: ObjectOneOf          =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _: ObjectHasValue       =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _: DataExactCardinality =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _: DataMaxCardinality   =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _: DataMinCardinality   =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _: DataAllValuesFrom    =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _: DataHasValue         =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _: DataSomeValuesFrom   =>
        featureObserver.reportUnsupported(ce)
        normalise(owlThing)

      case _ =>
        throw new AssertionError(s"$ce is not in a valid normal form.")
    })

  for ((lhs: ClassExpression, rhs: ClassExpression) <- transformedInput.getInternalizedAxioms) {
    val lhsFragments: Set[ClauseFragment] = normalise(lhs)
    val rhsFragments: Set[ClauseFragment] = normalise(rhs)
    val clauses: Set[STClause] =
      for ((body, head) <- combine(doCrossProduct = true)(lhsFragments, rhsFragments)) yield
        STClause(body, head)
    addToIndex(clauses)
  }
  for (AsymmetricObjectPropertyAxiom(ope) <- transformedInput.getAsymmetricProperties) {
    val z = zFresh()
    val role = getRole(z)(ope)
    indexOntologyClause(OntologyClause(Seq(role, role.inverse), Nil))
  }
  for (ReflexiveObjectPropertyAxiom(ope) <- transformedInput.getReflexiveProperties) {
    val role = getRole(x)(ope)
    indexOntologyClause(OntologyClause(Nil, Seq(role)))
  }
  for (SubObjectPropertyOfAxiom(lhs, rhs) <- transformedInput.getSimpleSubProperties) {
    val z = zFresh()
    val lhsRole = getRole(z)(lhs)
    val rhsRole = getRole(z)(rhs)
    indexOntologyClause(OntologyClause(Seq(lhsRole), Seq(rhsRole)))
  }
  for (DisjointObjectPropertiesAxiom(properties) <- transformedInput.getDisjointProperties) {
    val prop: Array[ObjectPropertyExpression] = properties.toArray
    val z = zFresh()
    val ps: Array[Role] = cmap (prop) (getRole(z))
    crange (0, ps.length) { i =>
      crange (i + 1, ps.length) { j => indexOntologyClause(OntologyClause(Seq(ps(i), ps(j)), Nil)) }
    }
  }

  def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    var first = true
    def append(clause: OntologyClause) =
      if (first) {
        b append clause
        first = false
      } else {
        b append sep
        b append clause
      }
    b append start
    for (clause <- facts) append(clause)
    val seen = new mutable.HashSet[OntologyClause]
    for (clause <- conceptClauses.valuesIterator) if (seen.add(clause)) append(clause)
    for (clause <- forwardRoleClauses.valuesIterator) if (seen.add(clause)) append(clause)
    for (clause <- backwardRoleClauses.valuesIterator) if (seen.add(clause)) append(clause)
    b append end
    b
  }

  override def toString: String = addString(new StringBuilder, "DLOntology[\n", "\n", "\n]").result

}
