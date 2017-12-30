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

package com.sequoiareasoner.cli

import java.io.File
import java.util.ArrayList

import com.sequoiareasoner.kernel.Reasoner
import com.sequoiareasoner.kernel.taxonomy.Taxonomy
import com.sequoiareasoner.owlapi.{SequoiaReasoner, SequoiaReasonerConfiguration}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLAxiom, OWLOntology, OWLOntologyManager}
import org.semanticweb.owlapi.reasoner.InferenceType
import org.semanticweb.owlapi.util.{InferredAxiomGenerator, InferredEquivalentClassAxiomGenerator, InferredOntologyGenerator, InferredSubClassAxiomGenerator}
import util.PeakMemoryUsage

/** CLI command to classify an ontology.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
final class CLICommandClassify extends CLICommand {

  override val taskName = "classify"

  override val shortDescription =
    "Classify the ontology and optionally output the hierarchy to file"

  override protected[this] val options: CommandOptionSet = {
    val options: CommandOptionSet = globalOptions
    val outFileOption = CommandOption(
      longOption = "output",
      shortOption = "o",
      description = "File name to output classification results.",
      isMandatory = false)
    outFileOption.setArg(RequiredArgument)
    options += outFileOption
    val disableMultithreadingOption = CommandOption(
      longOption = "disableMultithreading",
      shortOption = "dmt",
      description = "Disables multithreading.",
      isMandatory = false)
    disableMultithreadingOption.setArg(NoArgument)
    options += disableMultithreadingOption
    val disableEqualityReasoningOption = CommandOption(
      longOption = "disableEqualityReasoning",
      shortOption = "deq",
      description = "Disables equality reasoning (may lead to incompleteness).",
      isMandatory = false)
    disableEqualityReasoningOption.setArg(NoArgument)
    options += disableEqualityReasoningOption
    options += ignoreImportsOption
    options
  }

  /**
    * Performs classification.
    */
  override def run {
    val disableMultithreadingOption: CommandOption = options.getOption("disableMultithreading")
    val shouldEnableMultithreading: Boolean = !disableMultithreadingOption.getValueAsBoolean
    val disableEqualityReasoningOption: CommandOption = options.getOption("disableEqualityReasoning")
    val shouldEnableEqualityReasoning: Boolean = !disableEqualityReasoningOption.getValueAsBoolean

    val reasoner: SequoiaReasoner = getSequoiaReasoner(new SequoiaReasonerConfiguration(enableMultithreading = shouldEnableMultithreading, enableEqualityReasoning = shouldEnableEqualityReasoning))
    val internalReasoner: Reasoner = reasoner.getInternalReasoner

    startTask("consistency check")
    val isInconsistent: Boolean = internalReasoner.isInconsistent
    finishTask("consistency check")
    if (isInconsistent) throw ExceptionMessage("Ontology is inconsistent")
    startTask("classification")
    val taxonomy: Taxonomy = internalReasoner.getTaxonomy
    finishTask("classification")

    val outFileOption: CommandOption = options.getOption("output")
    val outFileName = outFileOption.getValueAsString
    if (outFileName != null) {
      verbose("Output file: " + outFileName)
      startTask("outputting classification to file")
      val outFile = new File(outFileName)

      val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager

      // Classify the ontology.
      reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY)

      // To generate an inferred ontology we use implementations of inferred axiom generators.
      val gens = new ArrayList[InferredAxiomGenerator[_ <: OWLAxiom]]()
      gens.add(new InferredSubClassAxiomGenerator)
      gens.add(new InferredEquivalentClassAxiomGenerator)

      // Put the inferred axioms into a fresh empty ontology.
      val infOnt: OWLOntology = manager.createOntology
      val iog = new InferredOntologyGenerator(reasoner, gens)
      iog.fillOntology(manager.getOWLDataFactory, infOnt)

      // Save the inferred ontology.
      manager.saveOntology(infOnt, new FunctionalSyntaxDocumentFormat, IRI.create(outFile.toURI))

      finishTask("outputting classification to file")
    }

    println
    println(PeakMemoryUsage.getPeakMemoryUsageInfo())

    // Terminate the CSO runtime used by the reasoner.
    reasoner.dispose

  }
}
