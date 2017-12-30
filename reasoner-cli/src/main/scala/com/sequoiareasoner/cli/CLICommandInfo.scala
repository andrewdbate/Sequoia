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
import java.util.{Collections, Set}

import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.FileDocumentSource
import org.semanticweb.owlapi.model.{IRI, MissingImportHandlingStrategy, OWLAnnotationValue, OWLImportsDeclaration, OWLLiteral, OWLOntology, OWLOntologyLoaderConfiguration, OWLOntologyManager}
import org.semanticweb.owlapi.profiles.{OWL2DLProfile, OWL2ELProfile, OWL2Profile, OWL2QLProfile, OWL2RLProfile}
import org.semanticweb.owlapi.util.DLExpressivityChecker
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary

/** CLI command to print statistics about an ontology, including the expressivity and number of axioms and classes.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
final class CLICommandInfo extends CLICommand {
  private[this] final val profiles = List(new OWL2ELProfile, new OWL2QLProfile, new OWL2RLProfile, new OWL2DLProfile, new OWL2Profile)

  override val taskName: String = "info"

  override val shortDescription: String =
    "Display information and statistics about an ontology."

  override protected[this] val options: CommandOptionSet = {
    val options: CommandOptionSet = new CommandOptionSet
    // Don't call getGlobalOptions() since we override the behaviour of verbose
    val helpOption = CommandOption(
      longOption = "help",
      shortOption = "h",
      description = "Print this message",
      isMandatory = false)
    helpOption.setArg(NoArgument)
    options += helpOption
    val verboseOption = CommandOption(
      longOption = "verbose",
      shortOption = "v",
      description = "More verbose output",
      isMandatory = false)
    verboseOption.setArg(NoArgument)
    options += verboseOption
    options += ignoreImportsOption
    options
  }

  def run {
    startTask("statistics collection")
    try {
      val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager
      val config: OWLOntologyLoaderConfiguration = new OWLOntologyLoaderConfiguration
      val baseOntology: OWLOntology = manager.createOntology
      manager.getIRIMappers.clear
      if (options.getOption("ignore-imports").getValueAsBoolean)
        config.setMissingImportHandlingStrategy(MissingImportHandlingStrategy.SILENT)
      else
        config.setMissingImportHandlingStrategy(MissingImportHandlingStrategy.THROW_EXCEPTION)
      addSingleFile(inputFile, manager, config)
      manager.removeOntology(baseOntology)
      printStats(manager)
    } catch {
      case e: Exception => throw ExceptionChained(e)
    } finally {
      finishTask("statistics collection")
    }
  }

  /**
   * @return true iff the file was loaded successfully
   */
  private[this] def addSingleFile(inputFile: String, manager: OWLOntologyManager, config: OWLOntologyLoaderConfiguration): Unit =
    try {
      // The following line ensure that the (possibly relative) inputFile is made absolute
      manager.loadOntologyFromOntologyDocument(new FileDocumentSource(new File(inputFile)), config)
    } catch {
      case e: Exception => throw ExceptionChained(e)
    }

  private[this] def printStats(manager: OWLOntologyManager): Unit =
    manager.getOntologies() forEach {
      ontology => {
        val ontologyLocationURI: IRI = manager.getOntologyDocumentIRI(ontology)
        var ontologyLocation: String = "ontology"
        // On Windows, instead of printing the path in the form "file:/C:/..." we want to print it as "C:\..."
        if (ontologyLocationURI != null) try {
          val f: File = new File(ontologyLocationURI.toURI)
          ontologyLocation = f.getAbsoluteFile.toString
        } catch {
          case _: IllegalArgumentException => // ontologyLocationURI is not a file, so just ignore.
        }
        val ontologyIRI = ontology.getOntologyID.getOntologyIRI
        val ontologyBaseIRI: String = if (ontologyIRI.isPresent) ontologyIRI.get.toQuotedString else ""
        if (ontologyBaseIRI.isEmpty)
          output(s"Information about $ontologyLocation")
        else
          output(s"Information about $ontologyLocation ($ontologyBaseIRI)")
        if (verbose) printOntologyHeader(ontology)
        val expressivityChecker: DLExpressivityChecker = new DLExpressivityChecker(Collections.singleton(ontology))
        output("OWL Profile = " + getProfile(ontology))
        output("DL Expressivity = " + expressivityChecker.getDescriptionLogicName)
        output("Axioms = " + ontology.getAxiomCount)
        output("Logical Axioms = " + ontology.getLogicalAxiomCount)
        output("GCI Axioms = " + ontology.getGeneralClassAxioms().size)
        output("Individuals = " + ontology.getIndividualsInSignature().size)
        output("Classes = " + ontology.getClassesInSignature().size)
        output("Object Properties = " + ontology.getObjectPropertiesInSignature().size)
        output("Data Properties = " + ontology.getDataPropertiesInSignature().size)
        output("Annotation Properties = " + ontology.getAnnotationPropertiesInSignature().size)
        val imports: Set[OWLImportsDeclaration] = ontology.getImportsDeclarations
        if (imports.size > 0) {
          output("Direct Imports:")
          var count: Int = 1
          imports forEach { imp => output(count + ": " + imp.getIRI) }
          count += 1
        }
        output("")
      }
    }

  private[this] def getProfile(ontology: OWLOntology): String =
    profiles collectFirst {
      case profile if profile.checkOntology(ontology).isInProfile => profile.getName
    } getOrElse "Unknown Profile"

  private[this] def printOntologyHeader(ontology: OWLOntology): Unit =
    ontology.getAnnotations forEach {
      annotation => {
        val property: IRI = annotation.getProperty.getIRI
        val value: OWLAnnotationValue = annotation.getValue
        if (property == OWLRDFVocabulary.OWL_VERSION_INFO.getIRI) verbose("Version Info = " + getString(value))
        else if (property == OWLRDFVocabulary.OWL_PRIOR_VERSION.getIRI) verbose("Prior Version Info = " + getString(value))
        else if (property == OWLRDFVocabulary.OWL_BACKWARD_COMPATIBLE_WITH.getIRI) verbose("Backward Compatible With = " + getString(value))
        else if (property == OWLRDFVocabulary.OWL_INCOMPATIBLE_WITH.getIRI) verbose("Incompatible With = " + getString(value))
      }
    }

  private[this] def getString(value: OWLAnnotationValue): String = value match {
    case literal: OWLLiteral => literal.getLiteral
    case _ => value.toString
  }
}
