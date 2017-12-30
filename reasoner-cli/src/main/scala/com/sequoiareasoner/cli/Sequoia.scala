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

import java.io.{PrintWriter, StringWriter}

import java.util.logging.Logger

import com.sequoiareasoner.owlapi.SequoiaReasoner
import org.semanticweb.owlapi.util.Version

/** Sequoia command line entry point.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object Sequoia {
  val logger: Logger = Logger.getLogger("com.sequoiareasoner.CLI")

  /** The name of the executable bat or shell script for launching the reasoner from the command line. */
  final val executableScriptName = "sequoia"

  /** The name of the system. */
  final val systemName = "Sequoia"

  /** The available commands. */
  private[this] val commands: Seq[CLICommand] =
    Seq(new CLICommandClassify, new CLICommandConsistency, new CLICommandUnsatisfiable, new CLICommandInfo)

  private[this] def getVersionInfo: Version = SequoiaReasoner.getReasonerVersion

  private[this] def version: Unit = println(getVersionInfo)

  private[this] def mainHelp {
    val buf = new StringBuilder
    val version: Version = getVersionInfo
    val lineSeparator: String = System.lineSeparator
    buf.append(s"Usage: $executableScriptName <command> [options] <file URI>...$lineSeparator")
    buf.append(s"$systemName command-line client, version ${version.getMajor}.${version.getMinor}.${version.getPatch}.${version.getBuild}.$lineSeparator")
    buf.append(s"Type '$executableScriptName help <command>' for help on a specific command.$lineSeparator")
    buf.append(lineSeparator)
    buf.append(s"Available commands:$lineSeparator")
    val commandNames = commands map { _.taskName }
    // Print names of commands in alphabetical order.
    commandNames.sorted foreach {
      cmd =>
        buf.append("\t")
        buf.append(cmd)
        buf.append(lineSeparator)
    }
    buf.append(lineSeparator)
    buf.append(s"$systemName is an OWL 2 ontology reasoner for SRIQ ontologies.$lineSeparator")
    buf.append("For more information, visit https://www.sequoiareasoner.com")
    println(buf.result)
    System.exit(0)
  }

  private[this] def getCommand(name: String): CLICommand = {
    val cmds: Seq[CLICommand] = commands filter { command  => command.taskName == name.toLowerCase}
    cmds match {
      case Seq() => throw ExceptionMessage(s"Unrecognized command '$name'")
      case Seq(cmd) => return cmd
      case _ => throw new IllegalStateException(s"Ambiguous command '$name'")
    }
  }

  private[this] def run(args: Array[String]): Unit = {
    if (args.length == 0)
      throw ExceptionMessage(s"Type '$executableScriptName help' for usage.")
    val arg: String = args(0)
    if (arg == "h" || arg == "-h" || arg == "help" || arg == "--help") {
      if (args.length == 1) {
        mainHelp
      } else {
        val cmd: CLICommand = getCommand(args(1))
        cmd.help
      }
    } else if (arg == "--version" || arg == "-V") {
      version
    } else {
      val cmd: CLICommand = getCommand(arg)
      cmd.parseArgs(args)
      cmd.run
      cmd.finish
    }
  }

  private[this] def printError(e: Throwable): Unit =
    System.err.println(ExceptionFormatter.formatException(e))

  def main(args: Array[String]): Unit =
    try {
      run(args)
    } catch {
      case e: CLIException => {
        printError(e)
        val sw: StringWriter = new StringWriter
        e.printStackTrace(new PrintWriter(sw))
        logger.fine(sw.toString)
        logger.throwing(null, null, e)
        System.exit(1)
      }
    }

}