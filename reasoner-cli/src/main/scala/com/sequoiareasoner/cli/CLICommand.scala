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

import java.io.{File, StringWriter}
import java.util.Collections

import scala.collection.mutable
import com.sequoiareasoner.owlapi.{SequoiaReasoner, SequoiaReasonerConfiguration, SequoiaReasonerFactory}
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{OWLOntology, OWLOntologyCreationException, OWLOntologyManager}
import org.semanticweb.owlapi.util.DLExpressivityChecker
import util.Timers

/** Provides common functionality for Sequoia CLI commands.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
abstract class CLICommand {
  protected[this] var inputFile: String = null
  protected[this] var verbose: Boolean = false
  protected[this] val timers = new Timers

  def shortDescription: String

  lazy val taskUsage: String =
    s"${Sequoia.executableScriptName} $taskName $mandatoryOptions [options] <file URI>...".replaceAll(" +", " ")

  def taskName: String

  protected[this] val options: CommandOptionSet

  private[this] lazy val helpText: String = {
    val lineSeparator: String = System.lineSeparator
    val u = new StringBuilder
    val table = new CommandHelpTable(options)
    u.append("Description: " + shortDescription + lineSeparator + lineSeparator)
    u.append("Usage: " + taskUsage + lineSeparator + lineSeparator)
    u.append(table.print + lineSeparator)
    u.result
  }

  def requiresInputFiles: Boolean = true

  protected[this] def verbose(msg: => String): Unit = if (verbose) System.err.println(msg)

  protected[this] def output(msg: => String): Unit = println(msg)

  def run: Unit

  def finish: Unit =
    if (verbose) {
      val sw = new StringWriter
      timers.print(sw, true)
      verbose("")
      verbose("Timer summary:")
      verbose(sw.toString)
    }

  protected[this] def mandatoryOptions: String = {
    val ret = new StringBuilder
    val mandatory: Predef.Set[CommandOption] = options.mandatoryOptions
    for (option: CommandOption <- mandatory)
      ret.append("-" + option.shortOption + " arg ")
    ret.result
  }

  protected[this] def ignoreImportsOption: CommandOption = {
    val option = CommandOption(
      longOption = "ignore-imports",
      shortOption = "ignore-imports",
      description = "Ignore imported ontologies",
      isMandatory = false,
      defaultValue = Some(false))
    option.setArg(NoArgument)
    option
  }

  protected[this] final def globalOptions: CommandOptionSet = {
    val options = new CommandOptionSet
    val helpOption = CommandOption(
      longOption = "help",
      shortOption = "h",
      description = "Print this message",
      isMandatory = false,
      defaultValue = Some(false))
    helpOption.setArg(NoArgument)
    options += helpOption
    val verboseOption = CommandOption(
      longOption = "verbose",
      shortOption = "v",
      description = "Print full stack trace for errors.",
      isMandatory = false,
      defaultValue = Some(false))
    verboseOption.setArg(NoArgument)
    options += verboseOption
    options
  }

  protected def getSequoiaReasoner(config: SequoiaReasonerConfiguration = new SequoiaReasonerConfiguration): SequoiaReasoner =
    try {
      val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager
      val factory: SequoiaReasonerFactory = SequoiaReasonerFactory.getInstance
      if (inputFile == null) throw new IllegalStateException("input file is null")
      verbose("Input file: " + inputFile)
      startTask("loading")
      /*
      TODO: add option to force use of a particular parser.
      val registry: OWLParserFactoryRegistry = OWLParserFactoryRegistry.getInstance
      val parsers = registry.getParserFactories
      import collection.JavaConverters._
      for (p <- parsers.asScala.clone) p match {
        case p: org.coode.owlapi.rdfxml.parser.RDFXMLParserFactory =>
          registry.unregisterParserFactory(p)
        case p: org.coode.owlapi.owlxmlparser.OWLXMLParserFactory =>
          registry.unregisterParserFactory(p)
        case p: org.coode.owlapi.functionalparser.OWLFunctionalSyntaxParserFactory =>

        case p: uk.ac.manchester.cs.owl.owlapi.turtle.parser.TurtleOntologyParserFactory =>
          registry.unregisterParserFactory(p)
        case p: de.uulm.ecs.ai.owlapi.krssparser.KRSS2OWLParserFactory =>
          registry.unregisterParserFactory(p)
        case p: org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxParserFactory =>
          registry.unregisterParserFactory(p)
        case p: org.coode.owlapi.oboformat.OBOFormatParserFactory =>
          registry.unregisterParserFactory(p)
        case _ =>
          throw new Error("Unexpected parser.")
      }
      */

      val ontology: OWLOntology = manager.loadOntologyFromOntologyDocument(new File(inputFile))
      val reasoner: SequoiaReasoner = factory.createNonBufferingReasoner(ontology, config)
      finishTask("loading")
      if (verbose) {
        val sb = new StringBuilder
        sb.append("Classes = " + ontology.getClassesInSignature().size + ", ")
        sb.append("Object Properties = " + ontology.getObjectPropertiesInSignature().size + ", ")
        sb.append("Individuals = " + ontology.getIndividualsInSignature().size)
        verbose("Input size: " + sb)
        val expressivityChecker: DLExpressivityChecker = new DLExpressivityChecker(Collections.singleton(ontology))
        verbose("Expressivity: " + expressivityChecker.getDescriptionLogicName)
      }
      return reasoner
    } catch {
      case e: RuntimeException =>
        throw ExceptionChained(e)
      case e: OWLOntologyCreationException =>
        throw ExceptionChained(e)
    }

  def parseArgs(args: Array[String]) {

    def nextOption(list: Seq[String]): Unit = {
      val seenOptions = new mutable.HashSet[CommandOption]
      def dropDashes(s: String): String = s dropWhile {_ == '-'}
      def isValidOption(s: String) = (s matches "-[^-]*|--[^-]*") && options.containsOption(dropDashes(s))
      def isValidArgument(s: String) = !isValidOption(s)
      def checkAndGet(optionString: String): CommandOption = {
        val option = getOption(optionString)
        if (seenOptions contains option)
          throw ExceptionMessage(s"Repeated use of option: $optionString")
        seenOptions.add(option)
        option
      }
      def getOption(s: String): CommandOption = options.getOption(dropDashes(s))
      list match {
        case Nil => // Done

        case last +: Nil if isValidArgument(last) =>
          // The input file is given as a URI at the end.
          inputFile = last

        case optionString +: arg +: tail if isValidOption(optionString) && isValidArgument(arg) =>
          val option = checkAndGet(optionString)
          option.getArg match {
            case NoArgument =>
              option.setValue(true)
              nextOption(arg +: tail)
            case RequiredArgument | OptionalArgument =>
              option.setValue(arg)
              nextOption(tail)
          }

        case optionString +: tail if isValidOption(optionString) =>
          val option = checkAndGet(optionString)
          option.getArg match {
            case NoArgument =>
              option.setValue(true)
              nextOption(tail)
            case RequiredArgument | OptionalArgument =>
              throw ExceptionMessage(s"Option $optionString requires an argument")
          }

        case arg +: tail =>
          throw ExceptionMessage(s"Unexpected argument: $arg")
      }
    }

    // Skip first arg which is the name of the command.
    nextOption(args.toSeq.tail)

    // Check if all mandatory options are set.
    for (option <- options.allOptions if option.isMandatory && option.getValue == null)
      throw ExceptionMessage(s"Option ${option.longOption} is mandatory")

    if (options.getOption("verbose").getValueAsBoolean)
      verbose = true

    if (requiresInputFiles && inputFile == null)
      throw ExceptionMessage("No input file specified.")
  }

  def help {
    output(helpText)
    System.exit(0)
  }

  protected[this] def startTask(task: String): Unit = {
    verbose(s"Start $task")
    timers.startTimer(task)
  }

  protected[this] def finishTask(task: String): Unit = {
    val timer = timers.stopTimer(task)
    verbose(s"Finished $task in ${timer.format}")
  }

}
