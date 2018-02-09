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

import com.sequoiareasoner.buildinfo.BuildInfo
import com.sequoiareasoner.kernel.Reasoner
import com.sequoiareasoner.kernel.logging.Logger
import com.sequoiareasoner.owlapi.wrapper.OWLConverter
import com.sequoiareasoner.kernel.owl.iri.{IRI => SequoiaIRI}
import com.sequoiareasoner.kernel.reasoner.LoadingException
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.reasoner._
import org.semanticweb.owlapi.util.Version
import org.slf4j.{Logger => SLFLogger, LoggerFactory => SLFLoggerFactory}

object SequoiaReasoner {

  private object SequoiaLogger extends Logger {
    private[this] val logger: SLFLogger = SLFLoggerFactory.getLogger(classOf[SequoiaReasoner])
    override def config(msg: String): Unit = logger.info(msg)
    override def trace(msg: String): Unit = logger.trace(msg)
    override def info(msg: String): Unit = logger.info(msg)
    override def warn(msg: String): Unit = logger.warn(msg)
    override def error(msg: String): Unit = logger.error(msg)
  }

  def getReasonerName: String = BuildInfo.name

  def getReasonerVersion: Version =
    new Version(BuildInfo.major, BuildInfo.minor, BuildInfo.patch, 0)

}

/**
  * [[org.semanticweb.owlapi.reasoner.OWLReasoner]] interface implementation for [[Reasoner]].
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param ontology
  * @param isBufferingMode `true` iff the buffering mode for reasoner is `BufferingMode#BUFFERING`
  */
class SequoiaReasoner private[owlapi](ontology: OWLOntology, isBufferingMode: Boolean, config: SequoiaReasonerConfiguration) extends OWLReasoner {
  import SequoiaReasoner.SequoiaLogger

  private[this] def handleMethod = config.getUnsupportedAPIMethodHandler.handleMethod _
  private[this] def handleMethodWithDetail = config.getUnsupportedAPIMethodHandler.handleMethodWithDetail _

  /** Listener to implement addition and removal of axioms. */
  protected[this] object OntologyChangeListener extends OWLOntologyChangeListener {
    override def ontologiesChanged(changes: java.util.List[_ <: OWLOntologyChange]): Unit =
      changes forEach {
        change: OWLOntologyChange => {
          if (change.getOntology != ontology) {
            SequoiaLogger.warn(s"Ignoring change [${change}] because it is not applicable to the current ontology [${ontology}].")
          } else {
            SequoiaLogger.trace(s"Change pending: ${change}")
            pendingChanges enqueue change
            if (!change.isAxiomChange) ontologyReloadRequired = true
          }
          if (!isBufferingMode) flush
        }
      }
  }

  ontology.getOWLOntologyManager.addOntologyChangeListener(OntologyChangeListener)

  /** Progress monitor implementation to display progress. */
  private[this] val progressMonitor: SequoiaReasonerProgressMonitor = config.getSequoiaProgressMonitor

  /** `true` if it is required to reload the entire ontology next time the changes should be flushed. */
  private[this] var ontologyReloadRequired: Boolean = false

  /** The internal reasoner instance used for reasoning. */
  private[this] val reasoner = new Reasoner(config.getConfiguration, SequoiaLogger, config.getUnsupportedFeatureObserver(SequoiaLogger))

  /**
    * Loads the given [[org.semanticweb.owlapi.model.OWLOntology]] into the reasoner.
    */
  protected[this] def loadOntology: Unit = {
    //reasoner.clearState
    // Ontologies in the import closure.
    val importsClosure = ontology.getImportsClosure
    // The number of ontologies in the import closure.
    val importsClosureCount: Int = importsClosure.size
    // The current number of processed import closures.
    var importsClosureProcessed: Int = 0
    // Returns the current status of the process monitor.
    def getStatus(importsClosureProcessed: Int): String =
      if (importsClosureCount == 1) ReasonerProgressMonitor.LOADING
      else ReasonerProgressMonitor.LOADING + " " + (importsClosureProcessed + 1) + " of " + importsClosureCount
    val unsupportedSWRLRuleHandler = config.getUnsupportedSWRLRuleHandler(SequoiaLogger)
    importsClosure forEach { onto: OWLOntology =>
      progressMonitor.start(getStatus(importsClosureProcessed))
      val axioms: java.util.Set[OWLAxiom] = onto.getAxioms() // The axioms of the current imported ontology.
      val axiomsCount = axioms.size // The total number of axioms.
      var axiomsProcessed = 0 // Current number of processed axioms.
      axioms forEach {
        case rule: SWRLRule =>
          unsupportedSWRLRuleHandler.handleSWRLRule(rule)
        case axiom: OWLAxiom =>
          //logger.trace("loading " + axiom)
          if (OWLConverter.isRelevantAxiom(axiom)) reasoner.addAxiom(OWLConverter.convert(axiom))
          axiomsProcessed += 1
          progressMonitor.report(axiomsProcessed, axiomsCount)
      }
      importsClosureProcessed += 1
      progressMonitor.finish
    }
    progressMonitor.start("Structural Transformation")
    reasoner.performStructuralTransformation
    progressMonitor.finish
  }

  loadOntology // OWL API reasoners load the ontology at creation time.

  /**
    * Exposes the reasoner used internally in this OWL API wrapper.
    */
  def getInternalReasoner: Reasoner = reasoner

  override def dispose {
    ontology.getOWLOntologyManager.removeOntologyChangeListener(OntologyChangeListener)
    reasoner.shutdown
  }

  private[this] def getClassNode(iri: SequoiaIRI): Node[OWLClass] = TaxonomyNodeConverter.convertClassNode(reasoner.getClassNode(iri))

  override def getBottomClassNode: Node[OWLClass] =
    getClassNode(SequoiaIRI.owlNothing)

  override def getBottomDataPropertyNode: Node[OWLDataProperty] =
    handleMethod("getBottomDataPropertyNode():Node[OWLDataProperty]")

  override def getBottomObjectPropertyNode: Node[OWLObjectPropertyExpression] =
    handleMethod("getBottomObjectPropertyNode():Node[OWLObjectPropertyExpression]")

  override def getBufferingMode: BufferingMode = if (isBufferingMode) BufferingMode.BUFFERING else BufferingMode.NON_BUFFERING

  override def getDataPropertyDomains(arg0: OWLDataProperty, arg1: Boolean): NodeSet[OWLClass] =
    handleMethod("getDataPropertyDomains(OWLDataProperty,boolean):NodeSet[OWLClass]")

  override def getDataPropertyValues(arg0: OWLNamedIndividual, arg1: OWLDataProperty): java.util.Set[OWLLiteral] =
    handleMethod("getDataPropertyValues(OWLNamedIndividual,OWLDataProperty):Set[OWLLiteral]")

  override def getDifferentIndividuals(arg0: OWLNamedIndividual): NodeSet[OWLNamedIndividual] =
    handleMethod("getDifferentIndividuals(OWLNamedIndividual):NodeSet[OWLNamedIndividual]")

  override def getDisjointClasses(arg0: OWLClassExpression): NodeSet[OWLClass] =
    handleMethod("getDisjointClasses(OWLClassExpression):NodeSet[OWLClass]")

  override def getDisjointDataProperties(arg0: OWLDataPropertyExpression): NodeSet[OWLDataProperty] =
    handleMethod("getDisjointDataProperties(OWLDataPropertyExpression):NodeSet[OWLDataProperty]")

  override def getDisjointObjectProperties(arg0: OWLObjectPropertyExpression): NodeSet[OWLObjectPropertyExpression] =
    handleMethod("getDisjointObjectProperties(OWLObjectPropertyExpression):NodeSet[OWLObjectPropertyExpression]")

  override def getEquivalentClasses(ce: OWLClassExpression): Node[OWLClass] =
    if (ce.isAnonymous)
      handleMethodWithDetail("getEquivalentClasses(OWLClassExpression):Node[OWLClass]", " does not support computation of classes equivalent to unnamed class expressions")
    else
      getClassNode(OWLConverter.convert(ce.asOWLClass.getIRI))

  override def getEquivalentDataProperties(arg0: OWLDataProperty): Node[OWLDataProperty] =
    handleMethod("getEquivalentDataProperties(OWLDataProperty):Node[OWLDataProperty]")

  override def getEquivalentObjectProperties(arg0: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] =
    handleMethod("getEquivalentObjectProperties(OWLObjectPropertyExpression):Node[OWLObjectPropertyExpression]")

  override def getFreshEntityPolicy: FreshEntityPolicy =
    config.getFreshEntityPolicy

  override def getIndividualNodeSetPolicy: IndividualNodeSetPolicy =
    IndividualNodeSetPolicy.BY_NAME

  override def getInstances(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLNamedIndividual] =
    handleMethod("getInstances(OWLClassExpression,boolean):NodeSet[OWLNamedIndividual]")

  override def getInverseObjectProperties(arg0: OWLObjectPropertyExpression): Node[OWLObjectPropertyExpression] =
    handleMethod("getInverseObjectProperties(OWLObjectPropertyExpression):Node[OWLObjectPropertyExpression]")

  override def getObjectPropertyDomains(arg0: OWLObjectPropertyExpression, arg1: Boolean): NodeSet[OWLClass] =
    handleMethod("getObjectPropertyDomains(OWLObjectPropertyExpression,boolean):NodeSet[OWLClass]")

  override def getObjectPropertyRanges(arg0: OWLObjectPropertyExpression, arg1: Boolean): NodeSet[OWLClass] =
    handleMethod("getObjectPropertyRanges(OWLObjectPropertyExpression,boolean):NodeSet[OWLClass]")

  override def getObjectPropertyValues(arg0: OWLNamedIndividual, arg1: OWLObjectPropertyExpression): NodeSet[OWLNamedIndividual] =
    handleMethod("getObjectPropertyValues(OWLNamedIndividual,OWLObjectPropertyExpression):NodeSet[OWLNamedIndividual]")

  override def getPrecomputableInferenceTypes: java.util.Set[InferenceType] =
    new java.util.HashSet[InferenceType](java.util.Arrays.asList(InferenceType.CLASS_ASSERTIONS, InferenceType.CLASS_HIERARCHY))

  override def getReasonerName: String = SequoiaReasoner.getReasonerName

  override def getReasonerVersion: Version = SequoiaReasoner.getReasonerVersion

  override def getRootOntology: OWLOntology = ontology

  override def getSameIndividuals(arg0: OWLNamedIndividual): Node[OWLNamedIndividual] =
    handleMethod("getSubObjectProperties(OWLNamedIndividual):Node[OWLNamedIndividual]")

  override def getSubClasses(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLClass] =
    TaxonomyNodeConverter.convertClassNodes(reasoner.getSubClasses(OWLConverter.convert(ce), direct))

  override def getSubDataProperties(arg0: OWLDataProperty, arg1: Boolean): NodeSet[OWLDataProperty] =
    handleMethod("getSubDataProperties(OWLDataProperty,boolean):NodeSet[OWLDataProperty]")

  override def getSubObjectProperties(arg0: OWLObjectPropertyExpression, arg1: Boolean): NodeSet[OWLObjectPropertyExpression] =
    handleMethod("getSubObjectProperties(OWLObjectPropertyExpression,boolean):NodeSet[OWLObjectPropertyExpression]")

  override def getSuperClasses(ce: OWLClassExpression, direct: Boolean): NodeSet[OWLClass] = {
    // FIXME: this method should throw exception in the cases prescribed by the OWL API (e.g., inconsistent ontology)
    // We need to add tests to ensure that the exceptions get thrown when required.
    TaxonomyNodeConverter.convertClassNodes(reasoner.getSuperClasses(OWLConverter.convert(ce), direct))
  }

  override def getSuperDataProperties(arg0: OWLDataProperty, arg1: Boolean): NodeSet[OWLDataProperty] =
    handleMethod("getSuperDataProperties(OWLDataProperty,boolean):NodeSet[OWLDataProperty]")

  override def getSuperObjectProperties(arg0: OWLObjectPropertyExpression, arg1: Boolean): NodeSet[OWLObjectPropertyExpression] =
    handleMethod("getSuperObjectProperties(OWLObjectPropertyExpression,boolean):NodeSet[OWLObjectPropertyExpression]")

  override def getTimeOut: Long = 0

  override def getTopClassNode: Node[OWLClass] =
    getClassNode(SequoiaIRI.owlThing)

  override def getTopDataPropertyNode: Node[OWLDataProperty] =
    handleMethod("getTopDataPropertyNode:Node[OWLDataProperty]")

  override def getTopObjectPropertyNode: Node[OWLObjectPropertyExpression] =
    handleMethod("getTopObjectPropertyNode():Node[OWLObjectPropertyExpression]")

  override def getTypes(ind: OWLNamedIndividual, direct: Boolean): NodeSet[OWLClass] =
    handleMethod("getTypes(OWLNamedIndividual,boolean):NodeSet[OWLClass]")

  override def getUnsatisfiableClasses: Node[OWLClass] =
    getClassNode(SequoiaIRI.owlNothing)

  override def interrupt = reasoner.interrupt

  override def isConsistent: Boolean = !reasoner.isInconsistent

  override def isEntailed(arg0: OWLAxiom): Boolean =
    handleMethod("isEntailed(OWLAxiom):boolean")

  override def isEntailed(arg0: java.util.Set[_ <: OWLAxiom]): Boolean =
    handleMethod("isEntailed(Set[OWLAxiom]):boolean")

  override def isEntailmentCheckingSupported(arg0: AxiomType[_]): Boolean = false

  override def isPrecomputed(inferenceType: InferenceType): Boolean =
    if (inferenceType == InferenceType.CLASS_HIERARCHY) reasoner.isTaxonomyPrecomputed
    else false

  override def isSatisfiable(classExpression: OWLClassExpression): Boolean =
    reasoner.isSatisfiable(OWLConverter.convert(classExpression))

  override def precomputeInferences(inferenceTypes: InferenceType*) {
    try {
      if (inferenceTypes contains InferenceType.CLASS_HIERARCHY)
        reasoner.getTaxonomy
    } catch {
      case ex: Exception => throw ExceptionConverter.convert(ex)
    }
  }

  // [BEGIN] Methods to load changes to the ontology.

  /** the unprocessed changes to the ontology */
  private[this] val pendingChanges = new collection.mutable.Queue[OWLOntologyChange]

  override def flush: Unit = {
    if (ontologyReloadRequired) loadOntology
    if (pendingChanges.nonEmpty) {
      progressMonitor.start("Loading of Changes")
      val changesCount = pendingChanges.size
      //logger.trace(s"${status}: ${changesCount}")
      var currentAxiom = 0
      while (pendingChanges.nonEmpty) {
        val change = pendingChanges.dequeue
        if (!change.isAxiomChange) throw LoadingException("Cannot apply non-axiom change!")
        val owlAxiom = change.getAxiom
        val axiom = OWLConverter.convert(owlAxiom)
        if (change.isAddAxiom) {
          //logger.trace(s"Adding $owlAxiom.")
          reasoner.addAxiom(axiom)
        } else if (change.isRemoveAxiom) {
          //logger.trace(s"Removing $owlAxiom.")
          ???
          //reasoner.removeAxiom(axiom)
        }
        currentAxiom += 1
        progressMonitor.report(currentAxiom, changesCount)
      }
      progressMonitor.finish
    }
  }

  override def getPendingAxiomAdditions: java.util.Set[OWLAxiom] = {
    val additions = new java.util.HashSet[OWLAxiom]
    for (c <- pendingChanges)
      if (c.isAddAxiom) additions.add(c.getAxiom)
    additions
  }

  override def getPendingAxiomRemovals: java.util.Set[OWLAxiom] = {
    val removals = new java.util.HashSet[OWLAxiom]
    for (c <- pendingChanges)
      if (c.isRemoveAxiom) removals.add(c.getAxiom)
    removals
  }

  override def getPendingChanges: java.util.List[OWLOntologyChange] =
    collection.JavaConverters.mutableSeqAsJavaList(pendingChanges)

  // [END] Methods to load changes to the ontology.

}
