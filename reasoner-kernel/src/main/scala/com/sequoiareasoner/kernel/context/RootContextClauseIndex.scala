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

package com.sequoiareasoner.kernel.context

import com.sequoiareasoner.kernel.index.MutableIndexedSequence
import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.owl.iri.IRI

/** A context clause index that additionally maintains subsumption information. This class is intended to be used by
  * contexts that are introduced during context structure initialization in order to build the taxonomy.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param importedFacts
  */
final class RootContextClauseIndex(importedFacts: MutableIndexedSequence[IRI]) extends ContextClauseIndex {

  override def add(c: ContextClause): Unit = {
    super.add(c)
    if (c.body.length == 0) {
      if (c.head.length == 1) {
        c.head(0) match {
          case Concept(iri, t) if iri.isImported && t.isCentralVariable => importedFacts += iri
          case _ => // Do nothing
        }
      } else if (c.head.length == 0) {
        importedFacts += IRI.owlNothing
      }
    }
  }

}
