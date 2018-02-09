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

package com.sequoiareasoner.kernel.reasoner

import com.sequoiareasoner.kernel.taxonomy.Taxonomy
import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.owl.model._
import com.sequoiareasoner.kernel.{CommonNames, Reasoner}
import com.sequoiareasoner.kernel.logging.Logger
import com.sequoiareasoner.kernel.structural.UnsupportedFeatureObserverThrowException

import scala.collection.mutable

/** A collection of utility methods used by classification tests to print and compare computed taxonomies.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object ClassificationTestUtils {
  import CommonNames._

  def printClassAxioms(classIri: IRI, orderedEquivalentClassIris: Set[IRI], orderedSubClassIris: Set[IRI]): Set[Axiom] = {
    val result = new mutable.HashSet[Axiom]
    if (orderedEquivalentClassIris.size > 1)
      result += EquivalentClassesAxiom(orderedEquivalentClassIris map { iri => OWLClass(iri) })
    if (classIri != IRI.owlThing)
      for (subClass <- orderedSubClassIris if subClass != IRI.owlNothing)
        result += SubClassOfAxiom(OWLClass(subClass), OWLClass(classIri))
    result.toSet
  }

  def processTaxonomy(classTaxonomy: Taxonomy): Set[Axiom] =
    classTaxonomy.allNodes.flatMap{
      node =>
        val equivs: Set[IRI] = node.equivalentClasses.iterator.toSet
        def max(iris: Set[IRI]): IRI = iris.reduceLeft((x, y) => if (y <= x) x else y)
        // Take the largest IRI in the ordering as the representative.
        val representative = max(equivs)
        printClassAxioms(representative, equivs, node.directSubNodes.iterator.collect{
          case node if node != classTaxonomy.bottomNode => max(node.equivalentClasses.iterator.toSet)
        }.toSet)
    }.toSet

  /** Filter out those SubClassOf axioms asserting that a class is subsumed by another class which is itself equivalent
    * to owl:Thing.
    */
  def filterTopEquivalences(input: Set[Axiom]): Set[Axiom] =
    input filterNot {
      case SubClassOfAxiom(_, c2) => input exists {
        case EquivalentClassesAxiom(classes) => (classes contains c2) && (classes contains Thing)
        case _                               => false
      }
      case _ => false
    }

  class DecoratedSet(private val s: Set[Axiom]) {
    override def equals(o: Any): Boolean = o match {
      case that: DecoratedSet => this.s == that.s
      case _ => throw new IllegalArgumentException("Can only compare decorated sets.")
    }
    override def toString: String = s.toSeq.map{_.toString}.sorted.mkString(System.lineSeparator, System.lineSeparator, System.lineSeparator)
  }

  def decorate[A <: Axiom](s: Set[A]) = new DecoratedSet(s.toSet)

  private[this] val nopLogger = new Logger {
    override def config(msg: String): Unit = {}
    override def trace(msg: String): Unit = {}
    override def info(msg: String): Unit = {}
    override def warn(msg: String): Unit = {}
    override def error(msg: String): Unit = {}
  }

  /** Computes the taxonomy for the input set of axioms.
    *
    * @param input
    * @return None if the ontology is inconsistent, otherwise the taxonomy as Some set of axioms.
    */
  def computeTaxonomy(input: Set[_ <: Axiom]): Option[Set[Axiom]] = {
    // TODO: run the test for each kind of configuration.
    val config = ReasonerConfiguration(
      progressMonitor = new ProgressMonitor,
      enableMultithreading = true,
      enableEqualitySimplifyReflect = true,
      enableTrieRedundancyIndex = true,
      enableEqualityReasoning = true,
      allowFreshEntities = false
    )
    val reasoner = new Reasoner(config, nopLogger, DoNothingUnsupportedFeatureObserver)
    for (ax <- input) reasoner.addAxiom(ax)
    reasoner.performStructuralTransformation
    val result = if (!reasoner.isInconsistent) {
      val taxonomy: Taxonomy = reasoner.getTaxonomy
      Some(filterTopEquivalences(processTaxonomy(taxonomy)))
    } else {
      None
    }
    reasoner.shutdown
    result
  }
}
