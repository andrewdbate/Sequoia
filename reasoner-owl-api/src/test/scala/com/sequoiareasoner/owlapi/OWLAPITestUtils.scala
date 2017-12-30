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

package com.sequoiareasoner.owlapi

import java.io.InputStream

import org.semanticweb.owlapi.apibinding.OWLFunctionalSyntaxFactory.{EquivalentClasses, OWLNothing, OWLThing, Ontology}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner.{InconsistentOntologyException, InferenceType}
import org.semanticweb.owlapi.util.{InferredAxiomGenerator, InferredEquivalentClassAxiomGenerator, InferredOntologyGenerator, InferredSubClassAxiomGenerator}

import scala.language.postfixOps

object OWLAPITestUtils {
  import collection.JavaConverters._

  /** Assuming the OWLOntology represents a taxonomy (i.e. SubClassOf expressions with class expression literals only),
    * returns the taxonomy as a set of OWLSubClassAxioms.
    */
  private[this] def ontologyAsTaxonomy(ont: OWLOntology): Set[OWLSubClassOfAxiom] = {
    val taxonomy = ont.getAxioms(AxiomType.SUBCLASS_OF).asScala ++ (ont.getAxioms(AxiomType.EQUIVALENT_CLASSES).asScala flatMap { _.asOWLSubClassOfAxioms.asScala })
    val inconsistencyAxiom: Option[OWLSubClassOfAxiom] = taxonomy collectFirst {
      case axiom if axiom.getSubClass.isOWLThing &&  axiom.getSuperClass.isOWLNothing => axiom
    }
    inconsistencyAxiom match {
      case Some(inconsistency) =>
        // Discard all other subclass axioms except the one that shows the inconsistency.
        Set(inconsistency)
      case None =>
        // We do not have an inconsistency, so generate the taxonomy.
        taxonomy filterNot {
          // Filter out the axioms that we do not care about.
          axiom => axiom.getSuperClass.isOWLThing || axiom.getSubClass.isOWLNothing || axiom.getSubClass.equals(axiom.getSuperClass)
        } filterNot {
          // Filter out axioms whose subsumer is a class equivalent to owl:Thing.
          axiom => taxonomy exists { ax => ax.getSubClass.isOWLThing && ax.getSuperClass.equals(axiom.getSuperClass) }
        } toSet
    }
  }

  type Taxonomy = Set[OWLSubClassOfAxiom]

  private[this] def getComputedTaxonomy(input: InputStream): Taxonomy = {
    require(input != null)
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager
    val reasoner = createReasoner(input)
    val result = try {
      reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY)
      val gens = new java.util.ArrayList[InferredAxiomGenerator[_ <: OWLAxiom]]
      gens.add(new InferredSubClassAxiomGenerator)
      gens.add(new InferredEquivalentClassAxiomGenerator)
      // Put the inferred axioms into a fresh empty ontology.
      val taxonomy = manager.createOntology
      // Get the inferred ontology generator to generate the inferred axioms.
      val iog = new InferredOntologyGenerator(reasoner, gens)
      iog.fillOntology(manager.getOWLDataFactory, taxonomy)
      ontologyAsTaxonomy(taxonomy)
    } catch {
      case _: InconsistentOntologyException =>
        // Return the inconsistent ontology.
        ontologyAsTaxonomy(Ontology(manager, EquivalentClasses(OWLNothing, OWLThing)))
    }
    reasoner.dispose
    result
  }

  private[this] def getKnownTaxonomy(input: InputStream): Taxonomy = {
    val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager
    val known = manager.loadOntologyFromOntologyDocument(input)
    ontologyAsTaxonomy(known)
  }

  /**
    * @return a pair of resources, where the first component is the input to a test and the second is the expected test result.
    */
  def getComputedAndKnownInput(dir: String, name: String): (Taxonomy, Taxonomy) =
    (getComputedTaxonomy(getClass.getResourceAsStream(s"/$dir/$name.owl")),
      getKnownTaxonomy(getClass.getResourceAsStream(s"/$dir/$name.expected.owl")))

  private[this] def createReasoner(stream: InputStream): SequoiaReasoner = {
    require(stream != null)
    val manager = OWLManager.createOWLOntologyManager
    val ontology: OWLOntology = manager.loadOntologyFromOntologyDocument(stream)
    new SequoiaReasoner(ontology, false, SequoiaReasonerConfiguration.getDefaultOWLReasonerConfiguration())
  }

  /** A simple [[OWLOntologyIRIMapper]] that appends the provided root
    * directory to the ontologyIRI and creates an owl extension
    */
  private[this] class ThisIRIMapper(val root: String) extends OWLOntologyIRIMapper {
    override def getDocumentIRI(ontologyIRI: IRI): IRI = IRI.create(s"$root/${ontologyIRI.getShortForm}.owl")
  }

  /** Loads an ontology from the test resources.
    *
    * @param manager
    * @param root
    * @param name
    * @return the loaded ontology.
    */
  def loadOntology(manager: OWLOntologyManager, root: String, name: String): OWLOntology = {
    val ontologyRoot = getClass.getResource(s"/$root").toURI
    manager.getIRIMappers.add(new ThisIRIMapper(ontologyRoot.toString))
    val stream: InputStream = getClass.getResourceAsStream(s"/$root/$name")
    assert(stream != null)
    manager.loadOntologyFromOntologyDocument(stream)
  }

  /** Returns the axioms from the import closure of the given ontology.
    *
    * @return the axioms from the import closure of the given ontology.
    */
  def getAxioms(main: OWLOntology): Iterable[OWLAxiom] = main.getImportsClosure.asScala flatMap { ont => ont.getAxioms().asScala }

}
