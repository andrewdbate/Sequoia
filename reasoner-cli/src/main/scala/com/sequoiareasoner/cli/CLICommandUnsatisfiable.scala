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

package com.sequoiareasoner.cli

import com.sequoiareasoner.owlapi.SequoiaReasoner

/** CLI command to print the classes of an ontology that are unsatisfiable.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
final class CLICommandUnsatisfiable extends CLICommand {

  override val taskName = "unsatisfiable"

  override val shortDescription = "Find the unsatisfiable classes in the ontology"

  override protected[this] val options: CommandOptionSet = {
    val options: CommandOptionSet = globalOptions
    options += ignoreImportsOption
    options
  }

  def run = {
    val reasoner: SequoiaReasoner = getSequoiaReasoner()
    startTask("consistency check")
    val isConsistent: Boolean = reasoner.isConsistent
    finishTask("consistency check")
    if (!isConsistent)
      throw ExceptionMessage("Ontology is inconsistent.")
    startTask("finding unsatisfiable")
    val unsatisfiableClasses = reasoner.getUnsatisfiableClasses.getEntities
    finishTask("finding unsatisfiable")
    output("")
    // Total number of unsatisfiable classes excluding owl:Nothing.
    val numUnsatisfiableClasses = unsatisfiableClasses.size - 1
    if (numUnsatisfiableClasses == 0) {
      output("Found no unsatisfiable concepts.")
    } else {
      val plural = if (numUnsatisfiableClasses > 1) "s" else ""
      output(s"Found $numUnsatisfiableClasses unsatisfiable concept$plural:")
      unsatisfiableClasses forEach { owlClass =>
        if (!owlClass.isOWLNothing) output(owlClass.toString)
      }
    }
    // Dispose of the reasoner to quickly terminate.
    reasoner.dispose
  }

}
