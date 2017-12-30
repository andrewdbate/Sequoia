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

object ReasonerConfiguration {

  /** Returns the default configuration for the Sequoia reasoner. This is the configuration that is appropriate for most
    * ontologies and general use.
    *
    * @return the default configuration for the Sequoia reasoner.
    */
  def getDefaultConfiguration: ReasonerConfiguration =
    ReasonerConfiguration(new ProgressMonitor, true, true, true, true, false)

  private def mkString(b: Boolean): String = if (b) "ON" else "OFF"

}

/** Configuration for the reasoner.
  *
  * Note that disabling equality reasoning may lead to incompleteness.
  *
  * @param progressMonitor                progress monitor used for reporting progress.
  * @param enableMultithreading           `true` iff multithreading should be enabled.
  * @param enableEqualitySimplifyReflect  `true` iff the simplify-reflect optimisation for equality reasoning should be enabled.
  * @param enableTrieRedundancyIndex      `true` iff the trie index should be used for clause redundancy checking.
  * @param enableEqualityReasoning        `true` iff equality reasoning should be enabled.
  * @param allowFreshEntities             `true` iff fresh entities are allowed in the signature of queries to the reasoner.
  */
final case class ReasonerConfiguration(progressMonitor: ProgressMonitor,
                                       enableMultithreading: Boolean,
                                       enableEqualitySimplifyReflect: Boolean,
                                       enableTrieRedundancyIndex: Boolean,
                                       enableEqualityReasoning: Boolean,
                                       allowFreshEntities: Boolean) {
  import ReasonerConfiguration.mkString

  override def toString: String =
    s"ReasonerConfiguration[ProgressMonitor: $progressMonitor; Multithreading: ${mkString(enableMultithreading)}; EqualitySimplifyReflect ${mkString(enableEqualitySimplifyReflect)}; TrieRedundancyIndex ${mkString(enableTrieRedundancyIndex)}; EqualityReasoning ${mkString(enableEqualityReasoning)}; AllowFreshEntities ${mkString(allowFreshEntities)}]"

}
