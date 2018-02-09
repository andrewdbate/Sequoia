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

package com.sequoiareasoner.kernel.reasoner

import com.sequoiareasoner.kernel.Reasoner
import com.sequoiareasoner.kernel.logging.Logger
import com.sequoiareasoner.kernel.owl.model.SubClassOfAxiom
import com.sequoiareasoner.kernel.structural.UnsupportedFeatureObserverIgnore
import com.sequoiareasoner.kernel.CommonNames
import org.scalatest.FunSuite

class ReasonerAPITest extends FunSuite {
  import CommonNames._

  private[this] val Configuration = ReasonerConfiguration.getDefaultConfiguration

  test("Fresh entities exception") {
    val reasoner = new Reasoner(Configuration copy (allowFreshEntities = false), DoNothingLogger, DoNothingUnsupportedFeatureObserver)
    reasoner.addAxiom(SubClassOfAxiom(A, B))
    reasoner.performStructuralTransformation
    reasoner.getClassNode(A)
    // An exception should be thrown on the fresh entity C.
    assertThrows[FreshEntitiesException]{
      reasoner.getClassNode(C)
    }
  }

  test("Fresh entities allowed") {
    val reasoner = new Reasoner(Configuration copy (allowFreshEntities = true), DoNothingLogger, DoNothingUnsupportedFeatureObserver)
    reasoner.addAxiom(SubClassOfAxiom(A, B))
    reasoner.performStructuralTransformation
    reasoner.getClassNode(A)
    // The fresh entity C should be allowed.
    reasoner.getClassNode(C)
  }

}
